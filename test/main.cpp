#define CATCH_CONFIG_MAIN

#include "catch.hpp"

#include "DataGraph.h"

using namespace afk;
using namespace std;

string inlineASMFunction(const std::string& funcName,
                         vector<string> asmOps) {
  string res = "void " + funcName + "(void* arg) {\n";

  res += "\t__asm__(\n";

  for (auto& op : asmOps) {
    res += "\t\t\"" + op + "\\n\"\n";
  }
  res += "\t);\n";

  res += "}";
  
  return res;
}

string inlineVoidASMFunction(const std::string& funcName,
                             vector<string> asmOps) {
  asmOps.push_back("leave");
  asmOps.push_back("ret");
  return inlineASMFunction(funcName, asmOps);
}

enum TestType {
  TEST_E,
  TEST_NE
};

enum ArithType {
  ARITH_INT_ADD,
  ARITH_FLOAT_ADD,
};

class Instruction {
public:
  virtual std::string toString() const { return "instruction!"; }
  virtual ~Instruction() {}
};

class Arithmetic : public Instruction {
protected:
  ArithType tp;
  int registerWidth;
  int opWidth;
  std::string source;
  std::string receiver;

public:
  Arithmetic(const ArithType tp_,
             const int registerWidth_,
             const int opWidth_,
             const std::string& source_,
             const std::string& receiver_) :
    tp(tp_),
    registerWidth(registerWidth_),
    opWidth(opWidth_),
    source(source_),
    receiver(receiver_) {}

  std::string toString() const {
    string opName;
    if (tp == ARITH_INT_ADD) {
      if ((registerWidth == 128) && (opWidth == 32)) {
        opName = "paddd ";
      } else if ((registerWidth == 32) && (opWidth == 32)) {
        opName = "addl ";
      } else {
        assert(false);
      }
    } else {
      assert(false);
    }
    return opName + "%" + source + ", %" + receiver;
  }
};

class Load : public Instruction {
protected:
  int offset;
  int alignment;
  int width;
  std::string receiver;

public:

  Load(const int offset_,
       const int alignment_,
       const int width_,
       const std::string& receiver_) :
    offset(offset_), alignment(alignment_), width(width_), receiver(receiver_) {}

  std::string toString() const {
    if (width == 128) {
      return "movdqu " + to_string(offset) + "(%rdi), %" + receiver;
    }

    if (width == 32) {
      return "movl " + to_string(offset) + "(%rdi), %" + receiver;
    }

    if (width == 16) {
      return "mov " + to_string(offset) + "(%rdi), %" + receiver;
    }

    assert(false);
  }

};

class Test : public Instruction {
protected:
  TestType tp;
  int width;
  std::string source;
  std::string receiver;

public:
  Test(const TestType tp_,
       const int width_,
       const std::string& source_,
       const std::string& receiver_) :
    tp(tp_), width(width_), source(source_), receiver(receiver_) {}

  std::string toString() const {

    assert(tp == TEST_NE);

    if (width == 16) {
      return "test " + string("%") + source + ", %" + receiver;
    }

    assert(false);
  }

};

class Mov : public Instruction {
protected:
  int width;
  std::string source;
  std::string receiver;

public:

  Mov(const int width_,
      const std::string& source_,
      const std::string& receiver_) :
    width(width_), source(source_), receiver(receiver_) {}

  std::string toString() const {
    if (width == 128) {
      return "movdqu " + string("%") + source + ", %" + receiver;
    }

    if (width == 32) {
      return "movdl " + string("%") + source + ", %" + receiver;
    }

    if (width == 16) {
      return "mov " + string("%") + source + ", %" + receiver;
    }

    assert(false);
  }

};

class CMov : public Instruction {
protected:
  int width;
  std::string source;
  std::string receiver;

public:

  CMov(const int width_,
      const std::string& source_,
      const std::string& receiver_) :
    width(width_), source(source_), receiver(receiver_) {}

  std::string toString() const {
    if (width == 128) {
      return "cmovdque " + string("%") + source + ", %" + receiver;
    }

    if (width == 32) {
      return "cmovle " + string("%") + source + ", %" + receiver;
    }

    if (width == 16) {
      return "cmove " + string("%") + source + ", %" + receiver;
    }

    assert(false);
  }

};

class Store : public Instruction {
protected:
  int offset;
  int alignment;
  int width;
  std::string source;

public:

  Store(const int offset_,
       const int alignment_,
       const int width_,
       const std::string& source_) :
    offset(offset_), alignment(alignment_), width(width_), source(source_) {}

  std::string toString() const {
    if (width == 128) {
      return string("movdqu ") + string("%") + source + ", " + to_string(offset) + "(%rdi)";
    }

    if (width == 32) {
      return string("movl ") + string("%") + source + ", " + to_string(offset) + "(%rdi)";
    }

    if (width == 16) {
      return string("mov ") + string("%") + source + ", " + to_string(offset) + "(%rdi)";
    }
    
    assert(false);
  }

};

class LowProgram {
protected:
  std::string name;
  vector<Instruction*> instructions;

public:

  LowProgram(const std::string& name_) : name(name_) {}

  std::string getName() const { return name; }

  const std::vector<Instruction*>& getInstructions() const { return instructions; }

  void addLoad(const int offset,
               const int alignment,
               const int width,
               const std::string& receiver) {
    instructions.push_back(new Load(offset, alignment, width, receiver));
  }

  void addMov(const std::string& src, const std::string& dest, const int width) {
    instructions.push_back(new Mov(width, src, dest));
  }

  void addTest(const TestType tp,
               const std::string& src,
               const std::string& dest,
               const int width) {
    instructions.push_back(new Test(tp, width, src, dest));
  }
  
  void addCMov(const std::string& src, const std::string& dest, const int width) {
    instructions.push_back(new CMov(width, src, dest));
  }
  
  void addStore(const int offset,
                const int alignment,
                const int width,
                const std::string& source) {
    instructions.push_back(new Store(offset, alignment, width, source));
  }
  
  void addArithmetic(const ArithType tp,
                     const int registerWidth,
                     const int opWidth,
                     const std::string& source,
                     const std::string& receiver) {
    instructions.push_back(new Arithmetic(tp, registerWidth, opWidth, source, receiver));
  }
  
  int size() const { return instructions.size(); }

  ~LowProgram() {
    for (auto& is : instructions) {
      delete is;
    }
  }

};

std::string buildASMProg(const LowProgram& prog) {
  vector<string> opStrings;
  for (auto iptr : prog.getInstructions()) {
    opStrings.push_back(iptr->toString());
  }
  return inlineVoidASMFunction(prog.getName(), opStrings);
}

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
  newProgram.addLoad(0, 1, 128, "xmm0");
  newProgram.addLoad(128 / 8, 1, 128, "xmm1");
  newProgram.addArithmetic(ARITH_INT_ADD, 128, 32, "xmm0", "xmm1");
  newProgram.addStore((128 / 8)*2, 1, 128, "xmm1");

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
  
}

struct RegisterAssignment {
  std::vector<DGNode*> topoOrder;
  std::map<DGNode*, std::string> registerAssignment;
  std::map<DGNode*, int> offsets;
};

RegisterAssignment assignRegisters(DataGraph& dg) {
  vector<DGNode*> nodeOrder;
  vector<DGNode*> ins = allInputs(dg);

  set<DGNode*> alreadyAdded;
  concat(nodeOrder, ins);
  for (auto& node : nodeOrder) {
    alreadyAdded.insert(node);
  }

  vector<DGNode*> nodesLeft;
  for (auto& node : dg.getNodes()) {
    if (!elem(node, alreadyAdded)) {
      nodesLeft.push_back(node);
    }
  }

  while (nodesLeft.size() > 0) {
    DGNode* nd = nullptr;

    cout << "Nodes left size = " << nodesLeft.size() << endl;

    for (auto& node : nodesLeft) {
      bool allInputsAdded = true;
      for (auto& input : dg.getInputs(node)) {
        if (!elem(input, alreadyAdded)) {
          allInputsAdded = false;
          break;
        }
      }

      if (allInputsAdded) {
        alreadyAdded.insert(node);
        nodeOrder.push_back(node);
        remove(node, nodesLeft);
        break;
      }
    }

  }

  map<DGNode*, int> layout;
  int offset = 0;
  for (auto& node : nodeOrder) {
    if (node->getType() == DG_INPUT) {
      layout[node] = offset;
      offset += toInput(node)->getLength() / 8;
    } else if (node->getType() == DG_OUTPUT) {
      layout[node] = offset;
      offset += toOutput(node)->getLength() / 8;
    }

  }

  // Horrible hack
  vector<string> x86_32Bit{"eax", "ecx", "edx", "edi", "esi", "ebx"};

  map<DGNode*, string> regAssignment;
  for (auto& node : nodeOrder) {
    assert(x86_32Bit.size() > 0);

    if (node->getType() == DG_INPUT) {
      string nextReg = x86_32Bit.back();
      x86_32Bit.pop_back();

      regAssignment.insert({node, nextReg});
    } else if (node->getType() == DG_OUTPUT) {
    } else if (node->getType() == DG_BINOP) {
      DGBinop* bp = static_cast<DGBinop*>(node);
      regAssignment.insert({node, regAssignment[bp->getOp1()]});
    } else if (node->getType() == DG_TRINOP) {
      DGTrinop* bp = toTrinop(node);

      string nextReg = x86_32Bit.back();
      x86_32Bit.pop_back();

      regAssignment.insert({node, nextReg});
    }
  }

  return {nodeOrder, regAssignment, layout};
}

LowProgram buildLowProgram(const std::string& name,
                           const DataGraph& dg,
                           RegisterAssignment& regAssign) {
  LowProgram prog(name);

  for (auto& node : regAssign.topoOrder) {
    if (node->getType() == DG_INPUT) {
      auto in = toInput(node);

      prog.addLoad(regAssign.offsets[in],
                   0,
                   in->getLength(),
                   regAssign.registerAssignment[in]);

    } else if (node->getType() == DG_OUTPUT) {
      auto out = toOutput(node);

      prog.addStore(regAssign.offsets[out],
                    0,
                    out->getLength(),
                    regAssign.registerAssignment[out->getInput()]);
    } else if (node->getType() == DG_BINOP) {

      auto bop = toBinop(node);

      // TODO: Remove hardcoding
      prog.addArithmetic(ARITH_INT_ADD,
                         32,
                         32,
                         regAssign.registerAssignment[bop->getOp0()],
                         regAssign.registerAssignment[bop->getOp1()]);
    } else if (node->getType() == DG_TRINOP) {
      auto trop = toTrinop(node);

      assert(trop->getOpName() == "mux");

      // TODO: Remove length hardcoding
      auto& ra = regAssign.registerAssignment;
      prog.addMov(ra[trop->getOp0()], ra[trop], 16);

      prog.addTest(TEST_NE, ra[trop->getOp2()], ra[trop->getOp2()], 16);

      prog.addCMov(ra[trop->getOp1()], ra[trop], 16);
        
    } else {
      assert(false);
    }
  }

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
  REQUIRE(ra[in0] == "ebx");

  REQUIRE(regOrder.size() == 4);

  LowProgram lowProg = buildLowProgram("target", dg, regAssign);

  string prog =
    buildASMProg(lowProg);

  cout << prog << endl;

}
