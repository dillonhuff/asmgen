#include <iostream>
#include <string>
#include <vector>

#define CATCH_CONFIG_MAIN

#include "catch.hpp"

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
  
}
