#define CATCH_CONFIG_MAIN

#include "catch.hpp"

#include "algorithm.h"

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
    return "movdqu " + to_string(offset) + "(%rdi), %" + receiver;
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
    return string("movdqu ") + string("%") + source + ", " + to_string(offset) + "(%rdi)";
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

enum DGType {
  DG_INPUT,
  DG_OUTPUT,
  DG_BINOP
};

class DGNode {
public:
  virtual DGType getType() const = 0;

  virtual ~DGNode() {}
};

class DGIn : public DGNode {

  std::string name;
  int length;

public:
  DGIn(const std::string& name_, const int length_) :
    name(name_), length(length_) {}

  std::string getName() const { return name; }

  void setName(const std::string& nn) { name = nn; }

  virtual DGType getType() const { return DG_INPUT; }

  int getLength() const { return length; }
};

class DGOut : public DGNode {
  std::string name;
  int length;
  DGNode* in;

public:

  DGOut(const std::string& name_, const int length_, DGNode* const in_) :
    name(name_), length(length_), in(in_) {}
  
  virtual DGType getType() const { return DG_OUTPUT; }

  void setName(const std::string& nn) { name = nn; }

  int getLength() const { return length; }
};

DGIn* toInput(DGNode* const node) {
  assert(node->getType() == DG_INPUT);

  return static_cast<DGIn*>(node);
}

DGOut* toOutput(DGNode* const node) {
  assert(node->getType() == DG_OUTPUT);

  return static_cast<DGOut*>(node);
}

class DGBinop : public DGNode {
  std::string op;
  DGNode* op0;
  DGNode* op1;
  
public:

  DGBinop(const std::string& op_,
          DGNode* const op0_,
          DGNode* const op1_) : op(op_), op0(op0_), op1(op1_) {}

  virtual DGType getType() const { return DG_BINOP; }

  DGNode* getOp0() const { return op0; }
  DGNode* getOp1() const { return op1; }
};

class DataGraph {
protected:
  std::vector<DGNode*> nodes;
  std::map<DGNode*, std::vector<DGNode*> > inEdges;
  std::map<DGNode*, std::vector<DGNode*> > outEdges;
  

public:

  std::vector<DGNode*> getNodes() const { return nodes; }

  std::vector<DGNode*> getInputs(DGNode* const nd) const {
    auto it = inEdges.find(nd);

    assert(it != std::end(inEdges));

    return it->second;
  }

  void insertNode(DGNode* nd) {
    nodes.push_back(nd);
    inEdges[nd] = {};
    outEdges[nd] = {};
  }

  DGIn* addInput(const std::string& name, const int width) {
    auto dgIn = new DGIn(name, width);

    insertNode(dgIn);

    return dgIn;
  }

  DGOut* addOutput(const std::string& name,
                   const int width,
                   DGNode* const input) {

    auto dgOut = new DGOut(name, width, input);

    insertNode(dgOut);

    return dgOut;
  }

  DGBinop* addBinop(const std::string& op,
                    DGNode* const op0,
                    DGNode* const op1) {
    auto dgOut = new DGBinop(op, op0, op1);

    insertNode(dgOut);

    return dgOut;
  }
  
  ~DataGraph() {
    for (auto& nd : nodes) {
      delete nd;
    }
  }
};

std::vector<DGNode*> allInputs(const DataGraph& dg) {
  vector<DGNode*> nds;
  for (auto& node : dg.getNodes()) {
    if (node->getType() == DG_INPUT) {
      nds.push_back(node);
    }
  }

  return nds;
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
      nd = nodesLeft.back();

      bool allInputsAdded = true;
      for (auto& input : dg.getInputs(nd)) {
        if (!elem(input, alreadyAdded)) {
          allInputsAdded = false;
          break;
        }
      }

      if (allInputsAdded) {
        alreadyAdded.insert(nd);
        nodeOrder.push_back(nd);
        nodesLeft.pop_back();
        break;
      }
    }

  }

  map<DGNode*, int> layout;
  int offset = 0;
  for (auto& node : nodeOrder) {
    if (node->getType() == DG_INPUT) {
      layout[node] = offset;
      offset += toInput(node)->getLength();
    } else if (node->getType() == DG_OUTPUT) {
      layout[node] = offset;
      offset += toOutput(node)->getLength();
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
    }
  }

  return {nodeOrder, regAssignment, layout};
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
}
