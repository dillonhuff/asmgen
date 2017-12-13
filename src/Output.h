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

// static inline std::vector<std::string>
// inlineVoidASMFunction(std::vector<std::string> asmOps) {
//   asmOps.push_back("leave");
//   asmOps.push_back("ret");

//   return asmOps;
// }

static inline std::string inlineVoidASMFunction(const std::string& funcName,
                                                const bool hasClk,
                                                std::vector<std::string> asmOps) {
  std::vector<std::string> prog;
  prog.push_back("push %r15");
  prog.push_back("push %r14");
  prog.push_back("push %r13");
  prog.push_back("push %r12");
  //prog.push_back("push %rbp");
  prog.push_back("push %rbx");
  //prog.push_back("push %rsi");

  concat(prog, asmOps);

  //prog.push_back("pop %rsi");
  prog.push_back("pop %rbx");
  //prog.push_back("pop %rbp");
  prog.push_back("pop %r12");
  prog.push_back("pop %r13");  
  prog.push_back("pop %r14");
  prog.push_back("pop %r15");  

  //prog.push_back("leave");
  //prog.push_back("retq");

  return inlineASMFunction(funcName, hasClk, prog);
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
