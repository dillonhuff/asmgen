#pragma once

#include <cassert>
#include <iostream>
#include <vector>

#include "DataGraph.h"
#include "RegisterAssignment.h"

using namespace std;

class LowOperand {
public:

  virtual std::string toString() const { return "NOPE"; }
};

class LowRegister : public LowOperand {
public:
  std::string name;
  int width;

  LowRegister(const std::string& name_,
              const int width_) : name(name_), width(width_) {}

  virtual std::string toString() const { return name; }
};

class LowImmediate : public LowOperand {
public:
  int value;

  LowImmediate(const int value_) : value(value_) {}

  virtual std::string toString() const { return "$" + std::to_string(value); }
};

class LowAddress : public LowOperand {
public:
  int offset;
  LowRegister* base;
  LowRegister* offsetReg;
  int offsetFactor;

  LowAddress(const int offset_,
             LowRegister* const base_,
             LowRegister* const offsetReg_,
             const int offsetFactor_) :
    offset(offset_), base(base_), offsetReg(offsetReg_), offsetFactor(offsetFactor_) {}

  virtual std::string toString() const {
    return std::to_string(offset) + "(" + base->toString() + ", " + offsetReg->toString() + ", " + std::to_string(offsetFactor) + ")";
  }

};

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

class Label : public Instruction {
  std::string name;

public:
  Label(const std::string& name_) : name(name_) {}
  virtual std::string toString() const { return name + ":"; }
};

enum JumpType {
  JUMP_E,
  JUMP_NE,
};

class Jump : public Instruction {
  JumpType tp;
  std::string target;
public:
  Jump(const JumpType tp_, const std::string& name_) : tp(tp_), target(name_) {}
  virtual std::string toString() const {
    if (tp == JUMP_E) {
      return "je " + target;
    }

    if (tp == JUMP_NE) {
      return "jne " + target;
    }

    assert(false);
  }
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
      } else if ((registerWidth == 64) && (opWidth == 64)) {
        opName = "add ";
      } else {
        assert(false);
      }
    } else if (tp == ARITH_INT_SUB) {
      if ((registerWidth == 32) && (opWidth == 32)) {
        opName = "subl ";
      } else if ((registerWidth == 16) && (opWidth == 16)) {
        opName = "sub ";
      } else if ((registerWidth == 64) && (opWidth == 64)) {
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
      return "movzwl " + to_string(offset) + "(%rdi), " + receiver;
    }

    if (width == 8) {
      return "movzbl " + to_string(offset) + "(%rdi), " + receiver;
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

    if ((width == 16) || (width == 32)) {
      return "test " + source + ", " + receiver;
    }

    assert(false);
  }

};

class Cmp : public Instruction {
protected:
  int width;
  std::string source;
  std::string receiver;

public:
  Cmp(const int width_,
      const std::string& source_,
      const std::string& receiver_) :
    width(width_), source(source_), receiver(receiver_) {}

  std::string toString() const {

    return "cmp " + source + ", " + receiver;

  }

};

class Mov : public Instruction {
protected:
  int width;
  LowOperand* source;
  LowOperand* receiver;

public:

  Mov(const int width_,
      LowOperand* source_,
      LowOperand* receiver_) :
    width(width_), source(source_), receiver(receiver_) {}

  std::string toString() const {
    if (width == 128) {
      return "movdqu " + source->toString() + ", " + receiver->toString();
    }

    if (width == 32) {
      return "movdl " + source->toString() + ", " + receiver->toString();
    }

    if (width == 16) {
      return "mov " + source->toString() + ", " + receiver->toString();
    }

    if (width == 8) {
      return "movzwl " + source->toString() + ", " + receiver->toString();
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
  std::string clkName;
  std::string clkLastName;
  bool hasClk;

public:

  LowProgram(const std::string& name_) : name(name_), hasClk(false) {}

  bool hasClock() const { return hasClk; }

  void setClock(const std::string& cn,
                const std::string& cln) {
    hasClk = true;
    clkName = cn;
    clkLastName = cln;
  }

  std::string getName() const { return name; }

  const std::vector<Instruction*>& getInstructions() const { return instructions; }

  void addLoad(const int offset,
               const int alignment,
               const int width,
               const std::string& receiver) {
    instructions.push_back(new Load(offset, alignment, width, receiver));
  }

  void addMov(LowOperand* src,
              LowOperand* dest,
              const int width) {
    instructions.push_back(new Mov(width, src, dest));
  }

  void addJump(const JumpType tp,
               const std::string& target) {
    instructions.push_back(new Jump(tp, target));
  }

  void addLabel(const std::string& name) {
    instructions.push_back(new Label(name));
  }

  void addTest(const TestType tp,
               const std::string& src,
               const std::string& dest,
               const int width) {
    instructions.push_back(new Test(tp, width, src, dest));
  }

  void addCmp(const std::string& src,
              const std::string& dest,
              const int width) {
    instructions.push_back(new Cmp(width, src, dest));
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
