#pragma once

#include <fstream>

#include "LowProgram.h"
#include "RegisterAssignment.h"

static inline std::string inlineASMFunction(const std::string& funcName,
                                            std::vector<std::string> asmOps) {
  std::string res = "void " + funcName + "(void* arg) {\n";

  res += "\t__asm__(\n";

  for (auto& op : asmOps) {
    res += "\t\t\"" + op + "\\n\"\n";
  }
  res += "\t);\n";

  res += "}";
  
  return res;
}

static inline std::string inlineVoidASMFunction(const std::string& funcName,
                                                std::vector<std::string> asmOps) {
  asmOps.push_back("leave");
  asmOps.push_back("ret");
  return inlineASMFunction(funcName, asmOps);
}

static inline std::string buildASMProg(const LowProgram& prog) {
  vector<string> opStrings;
  for (auto iptr : prog.getInstructions()) {
    opStrings.push_back(iptr->toString());
  }
  return inlineVoidASMFunction(prog.getName(), opStrings);
}

static inline int compileCode(RegisterAssignment& regAssign,
                LowProgram& lowProg) {

  std::string prog =
    buildASMProg(lowProg);
  
  std::ofstream outf("./test/gencode/" + lowProg.getName() + ".cpp");
  outf << "#include \"" + lowProg.getName() + ".h\"\n\n" + prog;
  outf.close();

  std::ofstream hd("./test/gencode/" + lowProg.getName() + ".h");

  hd << "#pragma once\n\n #include <stdint.h>\n\n" + layoutStructString(regAssign.offsets) + "\nvoid " + lowProg.getName() + "(void*);\n";
  hd.close();
  
  int res = system(("clang++ -std=c++11 -c ./test/gencode/" + lowProg.getName() + ".cpp").c_str());

  return res;
}

