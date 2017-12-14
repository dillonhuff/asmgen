#include "LowProgram.h"

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

  if (str == "%r9d") {
    return "%r9";
  }

  if (str == "%r8d") {
    return "%r8";
  }

  if (str == "%r7d") {
    return "%r7";
  }
  
  cout << "Unsupported register = " << str << endl;
  
  assert(false);
}

void appendLowProgram(const DataGraph& dg,
                      RegisterAssignment& regAssign,
                      std::vector<DGNode*>& topoOrder,
                      LowProgram& prog) {
  for (auto& node : topoOrder) {
    if (node->getType() == DG_INPUT) {
      auto in = toInput(node);

      prog.addLoad(regAssign.getOffset(in),
                   0,
                   in->getLength(),
                   regAssign.registerAssignment[in]);

    } else if (node->getType() == DG_OUTPUT) {
      auto out = toOutput(node);

      prog.addStore(regAssign.getOffset(out),
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

        auto ra = regAssign.registerAssignment;
        // TODO: Fix in register allocation
        prog.addCmp(regAssign.registerAssignment[bop->getOp0()],
                    regAssign.registerAssignment[bop->getOp1()],
                    16);

        prog.addMov(new LowImmediate(1),
                    new LowRegister(ra[bop], 16), 16);
        
        // TODO: Fix this in register allocation
        prog.addMov(new LowImmediate(0), new LowRegister("%eax", 16), 16);
        prog.addCMov("%eax", ra[bop], 16);
        
        
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
      prog.addMov(new LowRegister(ra[trop->getOp0()], 16),
                  new LowRegister(ra[trop], 16), 16);

      prog.addTest(TEST_NE, ra[trop->getOp2()], ra[trop->getOp2()], 16);

      prog.addCMov(ra[trop->getOp1()], ra[trop], 16);

    } else if (node->getType() == DG_CONSTANT) {
      auto& ra = regAssign.registerAssignment;

      auto cn = toConstant(node);
      prog.addMov(new LowImmediate(stoi(cn->valueString())),
                  new LowRegister(ra[node], 16),
                  16);

    } else if (node->getType() == DG_MEM_OUTPUT) {
      auto& ra = regAssign.registerAssignment;

      auto memOut = toMemOutput(node);
      int memOffset = regAssign.getOffset(memOut);

      auto waddrReg = ra[memOut->getWAddr()];

      cout << "Waddr = " << memOut->getWAddr()->toString() << endl;

      cout << "Register assignment = " << endl;
      for (auto& val : ra) {
        cout << val.first->toString() << " --> " << val.second << endl;
      }
      
      assert(waddrReg != "");

      std::string freshLabel = regAssign.freshLabel();

      prog.addTest(TEST_NE, ra[memOut->getWEn()], ra[memOut->getWEn()], 16);
      prog.addJump(JUMP_E, freshLabel);

      prog.addMov(new LowRegister(ra[memOut->getWData()], 16),
                  new LowAddress(memOffset,
                                 new LowRegister("%rdi", 64),
                                 new LowRegister(to64Bit(ra[memOut->getWAddr()]), 64),
                                 2),
                  16);

      // prog.addMov(ra[memOut->getWData()],
      //             to_string(memOffset) + "(%rdi, " + to64Bit(ra[memOut->getWAddr()]) + ", 2)",
      //             16);

      prog.addLabel(freshLabel);

    } else if (node->getType() == DG_MEM_INPUT) {

      auto& ra = regAssign.registerAssignment;

      auto memIn = toMemInput(node);
      int memOffset = regAssign.getOffset(memIn);

      //to_string(memOffset) + "(%rdi, " + to64Bit(ra[memIn->getRAddr()]) + ", 2)",
      prog.addMov(new LowAddress(memOffset,
                                 new LowRegister("%rdi", 64),
                                 new LowRegister(to64Bit(ra[memIn->getRAddr()]), 64),
                                 2),
                  new LowRegister(ra[memIn], 16),
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

  int stackSize = regAssign.getStackSize();

  prog.addArithmetic(ARITH_INT_SUB, 64, 64,
                     "$" + to_string(stackSize),
                     "%rsp");

  appendLowProgram(dg, regAssign, regAssign.topoOrder, prog);

  prog.addArithmetic(ARITH_INT_ADD, 64, 64,
                     "$" + to_string(stackSize),
                     "%rsp");
  
  return prog;
}

