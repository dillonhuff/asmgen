#define CATCH_CONFIG_MAIN

#include "catch.hpp"

#include "DataGraph.h"
#include "Output.h"
#include "LowProgram.h"

#include "coreir.h"
#include "coreir/libs/commonlib.h"

using namespace afk;
using namespace std;
using namespace CoreIR;

TEST_CASE("Build tiny program") {
  vector<string> asmOps;
  asmOps.push_back("movdqu (%rdi), %xmm0");
  asmOps.push_back("movdqu 4(%rdi), %xmm1");
  asmOps.push_back("paddd %xmm0, %xmm1");
  asmOps.push_back("movdqu %xmm1, 8(%rdi)");
  asmOps.push_back("leave");
  asmOps.push_back("ret");

  string str = inlineASMFunction("test_add_func", asmOps);

  cout << str << endl;
}

TEST_CASE("Build program from low representation") {
  LowProgram newProgram("simd_add");
  newProgram.addLoad(0, 1, 128, "%xmm0");
  newProgram.addLoad(128 / 8, 1, 128, "%xmm1");
  newProgram.addArithmetic(ARITH_INT_ADD, 128, 32, "%xmm0", "%xmm1");
  newProgram.addStore((128 / 8)*2, 1, 128, "%xmm1");

  REQUIRE(newProgram.size() == 4);

  string prog =
    buildASMProg(newProgram);

  cout << prog << endl;

  std::ofstream out("./test/gencode/" + newProgram.getName() + ".cpp");
  out << prog;
  out.close();

  std::ofstream hd("./test/gencode/" + newProgram.getName() + ".h");
  hd << "#pragma once\nvoid " + newProgram.getName() + "(void*);\n";
  hd.close();
  
  int res = system("clang++ -std=c++11 ./test/gencode/test_add.cpp ./test/gencode/simd_add.cpp");

  REQUIRE(res == 0);

  res = system("./a.out");

  REQUIRE(res == 0);
  
}

string to64Bit(const std::string& str) {

  if (str == "%r15d") {
    return "%r15";
  }
  
  if (str == "%r14d") {
    return "%r14";
  }
  
  if (str == "%r13d") {
    return "%r13";
  }

  if (str == "%r12d") {
    return "%r12";
  }

  if (str == "%r11d") {
    return "%r11";
  }
  
  assert(false);
}

void appendLowProgram(const DataGraph& dg,
                      RegisterAssignment& regAssign,
                      std::vector<DGNode*>& topoOrder,
                      LowProgram& prog) {
  for (auto& node : topoOrder) { //regAssign.topoOrder) {
    if (node->getType() == DG_INPUT) {
      auto in = toInput(node);

      prog.addLoad(regAssign.getOffset(in),
                   0,
                   in->getLength(),
                   regAssign.registerAssignment[in]);

    } else if (node->getType() == DG_OUTPUT) {
      auto out = toOutput(node);

      prog.addStore(regAssign.getOffset(out), //->toString()],
                    0,
                    out->getLength(),
                    regAssign.registerAssignment[out->getInput()]);
    } else if (node->getType() == DG_BINOP) {

      auto bop = toBinop(node);

      // TODO: Remove hardcoding

      string op = bop->getOpName();
      if ((op == "+") ||
          (op == " + ")) {
        prog.addArithmetic(ARITH_INT_ADD,
                           32,
                           32,
                           regAssign.registerAssignment[bop->getOp0()],
                           regAssign.registerAssignment[bop->getOp1()]);
      } else if ((op == "==") || (op == " == ")) {
        prog.addTest(TEST_E,
                     regAssign.registerAssignment[bop->getOp0()],
                     regAssign.registerAssignment[bop->getOp1()],
                     32);
        
      } else if ((op == "!=") || (op == " != ")) {
        prog.addTest(TEST_NE,
                     regAssign.registerAssignment[bop->getOp0()],
                     regAssign.registerAssignment[bop->getOp1()],
                     32);
        
      } else if ((op == "*") ||
          (op == " * ")) {

        auto v0 = regAssign.registerAssignment[bop->getOp0()];
        auto v1 = regAssign.registerAssignment[bop->getOp1()];

        cout << "operand 0 = " << bop->getOp0()->toString() << endl;
        cout << "operand 1 = " << bop->getOp1()->toString() << endl;

        assert(v0 != "");
        assert(v1 != "");

        prog.addArithmetic(ARITH_INT_MUL,
                           32,
                           32,
                           regAssign.registerAssignment[bop->getOp0()],
                           regAssign.registerAssignment[bop->getOp1()]);
      } else {
        cout << "No binop for " << bop->toString() << " with op string |" <<
          bop->getOpName() << "|" << endl;

        assert(false);
      }
    } else if (node->getType() == DG_TRINOP) {
      auto trop = toTrinop(node);

      assert(trop->getOpName() == "mux");

      // TODO: Remove length hardcoding
      auto& ra = regAssign.registerAssignment;
      prog.addMov(ra[trop->getOp0()], ra[trop], 16);

      prog.addTest(TEST_NE, ra[trop->getOp2()], ra[trop->getOp2()], 16);

      prog.addCMov(ra[trop->getOp1()], ra[trop], 16);

    } else if (node->getType() == DG_CONSTANT) {
      auto& ra = regAssign.registerAssignment;

      prog.addMov("$1", ra[node], 16);

    } else if (node->getType() == DG_MEM_OUTPUT) {
      auto& ra = regAssign.registerAssignment;

      auto memOut = toMemOutput(node);
      int memOffset = 0;

      auto waddrReg = ra[memOut->getWAddr()];

      cout << "Waddr = " << memOut->getWAddr()->toString() << endl;

      cout << "Register assignment = " << endl;
      for (auto& val : ra) {
        cout << val.first->toString() << " --> " << val.second << endl;
      }
      
      assert(waddrReg != "");
      
      prog.addMov(ra[memOut->getWData()],
                  to_string(memOffset) + "(%rdi, " + to64Bit(ra[memOut->getWAddr()]) + ", 2)",
                  16);

    } else if (node->getType() == DG_MEM_INPUT) {

      auto& ra = regAssign.registerAssignment;

      auto memIn = toMemInput(node);
      int memOffset = 0;

      //auto raddr = ra[memIn->getRAddr()];
      // cout << "Raddr = " << memIn->getRAddr()->toString() << endl;

      // cout << "Register assignment = " << endl;
      // for (auto& val : ra) {
      //   cout << val.first->toString() << " --> " << val.second << endl;
      // }
      
      //assert(waddrReg != "");
      
      prog.addMov(to_string(memOffset) + "(%rdi, " + to64Bit(ra[memIn->getRAddr()]) + ", 2)",
                  ra[memIn],
                  16);


    } else {
      cout << "Unsupported node " << node->toString() << endl;
      assert(false);
    }
  }
}

LowProgram buildLowProgram(const std::string& name,
                           const DataGraph& dg,
                           RegisterAssignment& regAssign) {
  LowProgram prog(name);

  appendLowProgram(dg, regAssign, regAssign.topoOrder, prog);

  return prog;
}

TEST_CASE("Test conditional move node") {
  DataGraph dg;
  DGIn* in0 = dg.addInput("in0", 16);
  DGIn* in1 = dg.addInput("in1", 16);
  DGIn* sel = dg.addInput("sel", 16);

  DGTrinop* mx = dg.addTrinop("mux", in0, in1, sel);

  DGOut* of = dg.addOutput("out", 16, mx);

  auto regAssign = assignRegisters(dg);
  vector<DGNode*> regOrder = regAssign.topoOrder;

  LowProgram lowProg = buildLowProgram("cond_move", dg, regAssign);

  string prog =
    buildASMProg(lowProg);

  cout << "Mux program" << endl;
  cout << prog << endl;

  std::ofstream out("./test/gencode/" + lowProg.getName() + ".cpp");
  out << prog;
  out.close();

  std::ofstream hd("./test/gencode/" + lowProg.getName() + ".h");
  hd << "#pragma once\nvoid " + lowProg.getName() + "(void*);\n";
  hd.close();
  
  int res = system(("clang++ -std=c++11 ./test/gencode/" + lowProg.getName() + ".cpp " + "./test/gencode/test_mux.cpp").c_str());

  REQUIRE(res == 0);

  res = system("./a.out");

  REQUIRE(res == 0);
  
}

TEST_CASE("Build program from dataflow graph") {
  DataGraph dg;
  DGIn* in0 = dg.addInput("in0", 32);
  DGIn* in1 = dg.addInput("in1", 32);
  DGBinop* op = dg.addBinop("+", in0, in1);
  DGOut* out = dg.addOutput("out", 32, op);

  auto regAssign = assignRegisters(dg);
  vector<DGNode*> regOrder = regAssign.topoOrder;

  std::map<DGNode*, std::string> ra =
    regAssign.registerAssignment;
  REQUIRE(ra[in0] == "%r15d");

  REQUIRE(regOrder.size() == 4);

  LowProgram lowProg = buildLowProgram("target", dg, regAssign);

  string prog =
    buildASMProg(lowProg);

  cout << prog << endl;

}

void addDAGNodes(const std::deque<vdisc>& topoOrder,
                 NGraph& g,
                 map<Wireable*, DGNode*>& dgVerts,
                 DataGraph& dg) {
  for (auto& vd : topoOrder) {
    WireNode wd = g.getNode(vd);

    cout << "Converting " << nodeString(wd) << endl;

    if (isGraphInput(wd) && !isClkIn(*(wd.getWire()->getType()))) {
      auto in = dg.addInput(nodeString(wd), containerTypeWidth(*(wd.getWire()->getType())));

      dgVerts[wd.getWire()] = in;
    } else if (isGraphInput(wd) && isClkIn(*wd.getWire()->getType())) {
      continue;
    } else if (isGraphOutput(wd)) {

      auto inSels = getInputs(vd, g);
      cout << "Input selects size = " << inSels.size() << endl;
      
      assert(inSels.size() == 1);

      auto outp = dg.addOutput(wd.getWire()->toString(),
                               containerTypeWidth(*(wd.getWire()->getType())),
                               dgVerts[inSels[0]]);
      
    } else if (isConstant(wd)) {
      Instance* inst = toInstance(wd.getWire());

      // TODO: Add constant value computation
      auto dc = dg.addConstant(1, 16);

      dgVerts[inst->sel("out")] = dc;
    } else if (isInstance(wd.getWire())) {

      Instance* inst = toInstance(wd.getWire());

      if (isRegisterInstance(inst) && !wd.isReceiver) {
        auto in = dg.addInput(inst->toString(),
                              containerTypeWidth(*(inst->sel("out")->getType())));

        dgVerts[inst->sel("out")] = in;
      } else if (isRegisterInstance(inst) && wd.isReceiver) {

        auto inConns = getInputConnections(vd, g);

        InstanceValue in = findArg("in", inConns);

        auto outp = dg.addOutput(inst->toString(),
                                 containerTypeWidth(*(inst->sel("out")->getType())),
                                 dgVerts[in.getWire()]);

        //dgVerts[in.getWire()] = outp;
      } else if (isBitwiseOp(*inst) ||
                 isSignInvariantOp(*inst) ||
                 isUnsignedCmp(*inst) ||
                 isShiftOp(*inst) ||
                 isUDivOrRem(*inst)) {

        auto inConns = getInputConnections(vd, g);

        auto in0 = findArg("in0", inConns);
        auto in1 = findArg("in1", inConns);

        cout << "in0 = " << in0.getWire()->toString() << endl;
        cout << "in1 = " << in1.getWire()->toString() << endl;

        assert(dgVerts[in0.getWire()] != nullptr);
        assert(dgVerts[in1.getWire()] != nullptr);

        auto op = dg.addBinop(getOpString(*inst),
                              dgVerts[in0.getWire()],
                              dgVerts[in1.getWire()]);

        dgVerts[inst->sel("out")] = op;
      } else if (getQualifiedOpName(*inst) == "coreir.mux") {
        auto inConns = getInputConnections(vd, g);

        auto in0 = findArg("in0", inConns);
        auto in1 = findArg("in1", inConns);
        auto in2 = findArg("sel", inConns);

        auto op = dg.addTrinop("mux",
                               dgVerts[in0.getWire()],
                               dgVerts[in1.getWire()],
                               dgVerts[in2.getWire()]);

        dgVerts[inst->sel("out")] = op;
        
      } else if (getQualifiedOpName(*inst) == "corebit.term") {

        auto inConns = getInputConnections(vd, g);
        auto in = findArg("in", inConns);

        auto outp = dg.addOutput(inst->toString(),
                                 8,
                                 dgVerts[in.getWire()]);
        dgVerts[inst] = outp;

      } else if (isMemoryInstance(inst)) {
        if (wd.isReceiver) {

          auto inConns = getInputConnections(vd, g);
          auto waddr = findArg("waddr", inConns);
          auto wdata = findArg("wdata", inConns);
          
          auto in = dg.addMemOutput(inst->toString(),
                                    dgVerts[waddr.getWire()],
                                    dgVerts[wdata.getWire()],
                                    10*2,
                                    2);

        } else {

          auto inConns = getInputConnections(vd, g);
          auto raddr = findArg("raddr", inConns);
          
          auto in = dg.addMemInput(inst->toString(),
                                   dgVerts[raddr.getWire()],
                                   10*2,
                                   2);

          dgVerts[inst->sel("rdata")] = in;
        }
      } else {
        cout << "Unsupported instance = " << nodeString(wd) << endl;
        assert(false);
      }
    } else {
      cout << "Unsupported node = " << nodeString(wd) << endl;
      assert(false);
    }
  }
}

std::vector<DataGraph> coreModuleToDG(Module* m) {
  NGraph g;
  buildOrderedGraph(m, g);
  deque<vdisc> topoOrder = topologicalSort(g);

  CircuitPaths paths = buildCircuitPaths(topoOrder, g, *m);

  cout << "paths.preSequentialAlwaysDAGs.size() = " << paths.preSequentialAlwaysDAGs.size() << endl;
  cout << "paths.postSequentialAlwaysDAGs.size() = " << paths.postSequentialAlwaysDAGs.size() << endl;

  cout << "paths.preSequentialDAGs.size() = " << paths.preSequentialDAGs.size() << endl;
  cout << "paths.postSequentialDAGs.size() = " << paths.postSequentialDAGs.size() << endl;
  
  map<Wireable*, DGNode*> dgVerts;
  vector<DataGraph> dgs;

  for (auto& dag : paths.preSequentialDAGs) {
    assert(dag.nodes.size() == 1);

    DataGraph dg;
    addDAGNodes(addInputs(dag.nodes[0], g), g, dgVerts, dg);

    dgs.push_back(dg);
  }

  for (auto& dag : paths.postSequentialDAGs) {
    assert(dag.nodes.size() == 1);

    DataGraph dg;
    addDAGNodes(addInputs(dag.nodes[0], g), g, dgVerts, dg);
    dgs.push_back(dg);
  }

  cout << "# of dags = " << dgs.size() << endl;
  
  return dgs;
}

TEST_CASE("code from conv_3_1") {
  Context* c = newContext();
  CoreIRLoadLibrary_commonlib(c);

  if (!loadFromFile(c,"./test/conv_3_1.json")) {
    cout << "Could not Load from json!!" << endl;
    c->die();
  }

  c->runPasses({"rungenerators","flattentypes","flatten", "wireclocks-coreir"});

  Module* m = c->getGlobal()->getModule("DesignTop");
  
  // auto dg = coreModuleToDG(m)[0];

  // auto regAssign = assignRegisters(dg);
  // vector<DGNode*> regOrder = regAssign.topoOrder;

  // std::map<DGNode*, std::string> ra =
  //   regAssign.registerAssignment;

  // LowProgram lowProg = buildLowProgram("conv_3_1", dg, regAssign);

  // string prog =
  //   buildASMProg(lowProg);

  // cout << prog << endl;

  // cout << "Layout struct" << endl;
  // cout << layoutStructString(regAssign.offsets) << endl;
  
  // std::ofstream outf("./test/gencode/" + lowProg.getName() + ".cpp");
  // outf << prog;
  // outf.close();

  // std::ofstream hd("./test/gencode/" + lowProg.getName() + ".h");
  // hd << "#pragma once\nvoid " + lowProg.getName() + "(void*);\n";
  // hd.close();
  
  // int res = system(("clang++ -std=c++11 -c ./test/gencode/" + lowProg.getName() + ".cpp").c_str());

  // REQUIRE(res == 0);
  
  deleteContext(c);
}

TEST_CASE("Single register printout") {

  Context* c = newContext();

  uint width = 16;

  Type* regType = c->Record({
      {"clk", c->Named("coreir.clkIn")},
        {"in_0", c->BitIn()->Arr(width)},
          {"in_1", c->BitIn()->Arr(width)},
            {"out_0", c->Bit()->Arr(width)},
              });

  Module* regComb =
    c->getGlobal()->newModuleDecl("regComb", regType);

  ModuleDef* def = regComb->newModuleDef();

  def->addInstance("add0", "coreir.add", {{"width", Const::make(c, width)}});
  def->addInstance("reg0", "coreir.reg", {{"width", Const::make(c, width)}});

  def->connect("self.in_0", "add0.in0");
  def->connect("self.in_1", "add0.in1");

  def->connect("add0.out", "reg0.in");

  def->connect("reg0.out", "self.out_0");

  def->connect("self.clk", "reg0.clk");

  regComb->setDef(def);

  vector<DataGraph> dgs = coreModuleToDG(regComb);

  assert(dgs.size() > 0);

  auto regAssign = assignRegisters(dgs[0]);
  LowProgram lowProg = buildLowProgram("reg_path", dgs[0], regAssign);

  string prog =
    buildASMProg(lowProg);

  cout << "Dag 0 program" << endl;
  cout << prog << endl;
  
  for (uint i = 1; i < dgs.size(); i++) {
    appendAssignRegisters(dgs[i], regAssign);
    auto tpSort = topologicalSort(dgs[i]);
    appendLowProgram(dgs[i], regAssign, tpSort, lowProg);

    prog =
      buildASMProg(lowProg);

    cout << "Dag " << i << " program" << endl;
    cout << prog << endl;
  }

  compileCode(regAssign, lowProg);

  deleteContext(c);
}
