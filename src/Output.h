#pragma once

#include <fstream>

#include "LowProgram.h"
#include "RegisterAssignment.h"

static inline std::string inlineASMFunction(const std::string& funcName,
                                            const bool hasClk,
                                            std::vector<std::string> asmOps) {
  std::string res = "void " + funcName + "(layout* arg) {\n";

  if (hasClk) {
    res += "if ((arg->self_clk == 1) && (arg->self_clk_last == 0)) {\n";
  }
  res += "\t__asm__(\n";

  for (auto& op : asmOps) {
    res += "\t\t\"" + op + "\\n\"\n";
  }
  res += "\t);\n";

  res += "}\n";

  if (hasClk) {
    res += "}\n";
  }
  
  return res;
}

static inline std::vector<std::string>
inlineVoidASMFunction(std::vector<std::string> asmOps) {
  asmOps.push_back("leave");
  asmOps.push_back("ret");

  return asmOps;
}

static inline std::string inlineVoidASMFunction(const std::string& funcName,
                                                const bool hasClk,
                                                std::vector<std::string> asmOps) {
  asmOps.push_back("leave");
  asmOps.push_back("ret");
  return inlineASMFunction(funcName, hasClk, asmOps);
}

static inline std::string buildASMProg(const LowProgram& prog) {
  vector<string> opStrings;
  for (auto iptr : prog.getInstructions()) {
    opStrings.push_back(iptr->toString());
  }

  return inlineVoidASMFunction(prog.getName(),
                               prog.hasClock(),
                               opStrings);
}

int compileCode(RegisterAssignment& regAssign,
                LowProgram& lowProg);

int compileCodeAndRun(RegisterAssignment& regAssign,
                      LowProgram& lowProg);
