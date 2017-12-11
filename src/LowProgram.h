#pragma once

#include <cassert>
#include <iostream>
#include <vector>

#include "DataGraph.h"
#include "RegisterAssignment.h"

using namespace std;

enum TestType {
  TEST_E,
  TEST_NE
};

enum ArithType {
  ARITH_INT_ADD,
  ARITH_INT_SUB,
  ARITH_INT_MUL,
  ARITH_FLOAT_ADD,
  ARITH_BIT_OR,
  ARITH_BIT_AND,
  ARITH_BIT_XOR
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
    } else if (tp == ARITH_INT_SUB) {
      if ((registerWidth == 32) && (opWidth == 32)) {
        opName = "addl ";
      } else if ((registerWidth == 16) && (opWidth == 16)) {
        opName = "sub ";
      } else {
        assert(false);
      }
      
    } else if (tp == ARITH_INT_MUL) {
      if ((registerWidth == 32) && (opWidth == 32)) {
        opName = "imul ";
      } else if ((registerWidth == 16) && (opWidth == 16)) {
        opName = "imul ";
      } else {
        assert(false);
      }

    } else {
      cout << "Unsupported op type = " << tp << endl;
      assert(false);
    }
    return opName + source + ", " + receiver;
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
      return "movdqu " + to_string(offset) + "(%rdi), " + receiver;
    }

    if (width == 32) {
      return "movl " + to_string(offset) + "(%rdi), " + receiver;
    }

    if (width == 16) {
      return "mov " + to_string(offset) + "(%rdi), " + receiver;
    }

    if (width == 8) {
      return "movzwl " + to_string(offset) + "(%rdi), " + receiver;
    }
    
    cout << "Unsupported width = " << width << endl;
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

    //if (tp == TEST_NE) {

    if ((width == 16) || (width == 32)) {
      return "test " + source + ", " + receiver;
    }

    assert(false);
    // } else if (tp == TEST_E) {

    //   if ((width == 16) || (width == 32)) {
    //     return "testne " + source + ", " + receiver;
    //   }
      
    //   assert(false);
    // }
    // assert(false);
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
      return "movdqu " + source + ", " + receiver;
    }

    if (width == 32) {
      return "movdl " + source + ", " + receiver;
    }

    if (width == 16) {
      return "mov " + source + ", " + receiver;
    }

    if (width == 8) {
      return "movzwl " + source + ", " + receiver;
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
      return "cmovdqune " + source + ", " + receiver;
    }

    if (width == 32) {
      return "cmovlne " + source + ", " + receiver;
    }

    if (width == 16) {
      return "cmovne " + source + ", " + receiver;
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
      return string("movdqu ") + source + ", " + to_string(offset) + "(%rdi)";
    }

    if (width == 32) {
      return string("movl ") + source + ", " + to_string(offset) + "(%rdi)";
    }

    if (width == 16) {
      return string("mov ") + source + ", " + to_string(offset) + "(%rdi)";
    }

    if (width == 8) {
      return string("mov ") + source + ", " + to_string(offset) + "(%rdi)";
    }
    
    assert(false);
  }

};

class LowProgram {
protected:
  std::string name;
  std::vector<Instruction*> instructions;

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


LowProgram buildLowProgram(const std::string& name,
                           const DataGraph& dg,
                           RegisterAssignment& regAssign);

void appendLowProgram(const DataGraph& dg,
                      RegisterAssignment& regAssign,
                      std::vector<DGNode*>& topoOrder,
                      LowProgram& prog);
