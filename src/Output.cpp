#include "Output.h"

void writeOutFiles(RegisterAssignment& regAssign,
                   LowProgram& lowProg) {
  std::string prog =
    buildASMProg(lowProg);
  
  std::ofstream outf("./test/gencode/" + lowProg.getName() + ".cpp");
  outf << "#include \"" + lowProg.getName() + ".h\"\n\n" + prog;
  outf.close();

  std::ofstream hd("./test/gencode/" + lowProg.getName() + ".h");

  hd << "#pragma once\n\n #include <stdint.h>\n\n" + layoutStructString(regAssign.offsets) + "\nvoid " + lowProg.getName() + "(layout*);\n";
  hd.close();

}

int compileCode(RegisterAssignment& regAssign,
                LowProgram& lowProg) {

  writeOutFiles(regAssign, lowProg);
  
  int res = system(("clang++ -std=c++11 -c ./test/gencode/" + lowProg.getName() + ".cpp").c_str());

  return res;
}

int compileCodeAndRun(RegisterAssignment& regAssign,
                      LowProgram& lowProg) {
  writeOutFiles(regAssign, lowProg);
  
  int res = system(("clang++ -std=c++11 ./test/gencode/" + lowProg.getName() + ".cpp " + "./test/gencode/test_" + lowProg.getName() + ".cpp").c_str());

  if (res != 0) {
    return res;
  }

  res = system("./a.out");

  return res;
  
}
