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

  string str = inlineASMFunction("test_add_func", false, asmOps);

  cout << str << endl;
}

// TEST_CASE("Build program from low representation") {
//   LowProgram newProgram("simd_add");
//   newProgram.addLoad(0, 1, 128, "%xmm0");
//   newProgram.addLoad(128 / 8, 1, 128, "%xmm1");
//   newProgram.addArithmetic(ARITH_INT_ADD, 128, 32, "%xmm0", "%xmm1");
//   newProgram.addStore((128 / 8)*2, 1, 128, "%xmm1");

//   REQUIRE(newProgram.size() == 4);

//   string prog =
//     buildASMProg(newProgram);

//   cout << prog << endl;

//   int r = compileCodeAndRun(regAssign, prog);

//   REQUIRE(r == 0);
  
  // std::ofstream out("./test/gencode/" + newProgram.getName() + ".cpp");
  // out << prog;
  // out.close();

  // std::ofstream hd("./test/gencode/" + newProgram.getName() + ".h");
  // hd << "#pragma once\nvoid " + newProgram.getName() + "(layout* lt);\n";
  // hd.close();
  
  // int res = system("clang++ -std=c++11 ./test/gencode/test_add.cpp ./test/gencode/simd_add.cpp");

  // REQUIRE(res == 0);

  // res = system("./a.out");

  // REQUIRE(res == 0);
  
//}

TEST_CASE("Register assignment conflict") {
  DataGraph dg;
  DGIn* a = dg.addInput("a", 16);
  DGIn* b = dg.addInput("b", 16);
  DGIn* c = dg.addInput("c", 16);

  DGBinop* add0 = dg.addBinop("+", a, b);
  DGBinop* add1 = dg.addBinop("+", c, b);

  DGOut* r1 = dg.addOutput("r1", 16, add0);
  DGOut* r2 = dg.addOutput("r2", 16, add1);

  auto regAssign = assignRegisters(dg);

  vector<DGNode*> regOrder = regAssign.topoOrder;

  LowProgram lowProg = buildLowProgram("two_adds", dg, regAssign);

  string prog =
    buildASMProg(lowProg);

  cout << "Two registers program" << endl;
  cout << prog << endl;

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);
  
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

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);
  
  // std::ofstream out("./test/gencode/" + lowProg.getName() + ".cpp");
  // out << prog;
  // out.close();

  // std::ofstream hd("./test/gencode/" + lowProg.getName() + ".h");
  // hd << "#pragma once\nvoid " + lowProg.getName() + "(layout*);\n";
  // hd.close();
  
  // int res = system(("clang++ -std=c++11 ./test/gencode/" + lowProg.getName() + ".cpp " + "./test/gencode/test_mux.cpp").c_str());

  // REQUIRE(res == 0);

  // res = system("./a.out");

  // REQUIRE(res == 0);
  
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

  cout << "Starting conversion" << endl;

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

      cout << "Constant = " << inst->toString() << endl;
      BitVector bv(0, 0);
      if (getQualifiedOpName(*inst) == "coreir.const") {
        cout << "coreir const" << endl;

        Values args = inst->getModArgs();
        auto valArg = args["value"];

        assert(valArg != nullptr);

        cout << "valArg = " << valArg->toString() << endl;
        bv = valArg->get<BitVector>();

      } else {

        cout << "corebit const" << endl;
        Values args = inst->getModArgs();
        auto valArg = args["value"];

        assert(valArg->getValueType() == inst->getContext()->Bool());

        bool bval = valArg->get<bool>();

        if (bval == true) {
          bv = BitVector(1, 1);
        } else {
          bv = BitVector(1, 0);
        }
      }

      cout << "Bit vector val = " << bv << endl;
      cout << "Bit vector constant value = " << bv.to_type<int>() << endl;
      auto dc = dg.addConstant(bv.to_type<int>(), bv.bitLength());

      cout << "Adding constant " << inst->sel("out")->toString() << " node " << dc->toString() << endl;
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
          auto wen = findArg("wen", inConns);

          Values args = inst->getModuleRef()->getGenArgs();

          auto wArg = args["width"];
          auto dArg = args["depth"];
        
          uint width = wArg->get<int>(); //16;
          uint depth = dArg->get<int>();
          
          auto in = dg.addMemOutput(inst->toString(),
                                    dgVerts[waddr.getWire()],
                                    dgVerts[wdata.getWire()],
                                    dgVerts[wen.getWire()],
                                    (width/8)*depth,
                                    width / 8);

          cout << "Adding memoutput = " << in->toString() << " ";
          cout << "for node " << nodeString(wd) << endl;
          
          

        } else {

          auto inConns = getInputConnections(vd, g);
          auto raddr = findArg("raddr", inConns);

          Values args = inst->getModuleRef()->getGenArgs();

          auto wArg = args["width"];
          auto dArg = args["depth"];
        
          uint width = wArg->get<int>(); //16;
          uint depth = dArg->get<int>();
          
          auto in = dg.addMemInput(inst->toString(),
                                   dgVerts[raddr.getWire()],
                                   (width/8)*depth,
                                   width/8);

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
    addDAGNodes(addConstants(addInputs(dag.nodes[0], g), g), g, dgVerts, dg);

    dgs.push_back(dg);
  }

  for (auto& dag : paths.postSequentialDAGs) {
    assert(dag.nodes.size() == 1);

    DataGraph dg;
    addDAGNodes(addConstants(addInputs(dag.nodes[0], g), g), g, dgVerts, dg);
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

  vector<DataGraph> dgs = coreModuleToDG(m);

  auto clk = dgs[0].addInput("self_clk", 8);
  auto clk_last = dgs[0].addInput("self_clk_last", 8);

  assert(dgs.size() > 0);

  auto regAssign = assignRegisters(dgs[0]);
  LowProgram lowProg = buildLowProgram("conv_3_1", dgs[0], regAssign);

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

  lowProg.setClock("self_clk",
                   "self_clk_last");

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);

  deleteContext(c);
  
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
  
  // deleteContext(c);
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

  auto clk = dgs[0].addInput("self_clk", 8);
  auto clk_last = dgs[0].addInput("self_clk_last", 8);

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

  // regAssign.addInput("self_clk", 8);
  // regAssign.addInput("self_clk_last", 8);

  lowProg.setClock("self_clk",
                   "self_clk_last");

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);

  deleteContext(c);
}

TEST_CASE("Memory") {
  Context* c = newContext();
  
  uint width = 16;
  uint depth = 2;
  uint index = 1;

  Type* memoryType = c->Record({
      {"clk", c->Named("coreir.clkIn")},
        {"write_data", c->BitIn()->Arr(width)},
          {"write_addr", c->BitIn()->Arr(index)},
            {"write_en", c->BitIn()},
              {"read_data", c->Bit()->Arr(width)},
                {"read_addr", c->BitIn()->Arr(index)}
    });

      
  Module* memory = c->getGlobal()->newModuleDecl("memory0", memoryType);
  ModuleDef* def = memory->newModuleDef();

  def->addInstance("m0",
                   "coreir.mem",
                   {{"width", Const::make(c, width)},{"depth", Const::make(c, depth)}});
  //      		       {{"init", Const::make(c, BitVector(width*depth,0))}});

  def->connect("self.clk", "m0.clk");
  def->connect("self.write_en", "m0.wen");
  def->connect("self.write_data", "m0.wdata");
  def->connect("self.write_addr", "m0.waddr");
  def->connect("self.read_data", "m0.rdata");
  def->connect("self.read_addr", "m0.raddr");

  memory->setDef(def);

  c->runPasses({"rungenerators"});

  vector<DataGraph> dgs = coreModuleToDG(memory);

  auto clk = dgs[0].addInput("self_clk", 8);
  auto clk_last = dgs[0].addInput("self_clk_last", 8);

  assert(dgs.size() > 0);

  auto regAssign = assignRegisters(dgs[0]);
  LowProgram lowProg = buildLowProgram("memory", dgs[0], regAssign);

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

  // regAssign.addInput("self_clk", 8);
  // regAssign.addInput("self_clk_last", 8);

  lowProg.setClock("self_clk",
                   "self_clk_last");

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);

  deleteContext(c);
  
}

TEST_CASE("Equals then register") {

  Context* c = newContext();

  uint width = 16;

  Type* regType = c->Record({
      {"clk", c->Named("coreir.clkIn")},
        {"in_sel", c->BitIn()->Arr(width)},
          {"in_1", c->BitIn()->Arr(width)},
            {"in_2", c->BitIn()->Arr(width)},
              {"out_0", c->Bit()->Arr(width)}
    });

  Module* regComb =
    c->getGlobal()->newModuleDecl("regComb", regType);

  ModuleDef* def = regComb->newModuleDef();

  def->addInstance("eq0", "coreir.eq", {{"width", Const::make(c, width)}});
  def->addInstance("const10",
                   "coreir.const",
                   {{"width", Const::make(c, width)}},
                   {{"value", Const::make(c, BitVector(16, 10))}});


  def->addInstance("reg0", "coreir.reg", {{"width", Const::make(c, width)}});
  def->addInstance("mux0", "coreir.mux", {{"width", Const::make(c, width)}});

  def->connect("self.in_sel", "eq0.in0");
  def->connect("const10.out", "eq0.in1");

  def->connect("eq0.out", "mux0.sel");

  def->connect("self.in_1", "mux0.in0");
  def->connect("self.in_2", "mux0.in1");

  def->connect("mux0.out", "reg0.in");

  def->connect("self.clk", "reg0.clk");

  def->connect("reg0.out", "self.out_0");

  regComb->setDef(def);

  vector<DataGraph> dgs = coreModuleToDG(regComb);

  auto clk = dgs[0].addInput("self_clk", 8);
  auto clk_last = dgs[0].addInput("self_clk_last", 8);

  assert(dgs.size() > 0);

  auto regAssign = assignRegisters(dgs[0]);
  LowProgram lowProg = buildLowProgram("reg_eq", dgs[0], regAssign);

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

  // regAssign.addInput("self_clk", 8);
  // regAssign.addInput("self_clk_last", 8);

  lowProg.setClock("self_clk",
                   "self_clk_last");

  int r = compileCodeAndRun(regAssign, lowProg);

  REQUIRE(r == 0);

  deleteContext(c);
}
