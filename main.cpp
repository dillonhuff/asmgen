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
};

class LowProgram {
protected:
  vector<Instruction*> instructions;

public:

  void addLoad(const int offset,
               const int alignment,
               const int width,
               const std::string& receiver) {
    instructions.push_back(new Instruction());
  }

  void addStore(const int offset,
                const int alignment,
                const int width,
                const std::string& source) {
    instructions.push_back(new Instruction());
  }
  
  void addArithmetic(const ArithType offset,
                     const int registerWidth,
                     const int opWidth,
                     const std::string& source,
                     const std::string& receiver) {
    instructions.push_back(new Instruction());
  }
  
  int size() const { return instructions.size(); }

  ~LowProgram() {
    for (auto& is : instructions) {
      delete is;
    }
  }

};

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
  LowProgram newProgram;
  newProgram.addLoad(0, 1, 128, "xmm0");
  newProgram.addLoad(128 / 8, 1, 128, "xmm1");
  newProgram.addArithmetic(ARITH_INT_ADD, 128, 32, "xmm0", "xmm1");
  newProgram.addStore((128 / 8)*2, 1, 128, "xmm1");

  REQUIRE(newProgram.size() == 4);

}
