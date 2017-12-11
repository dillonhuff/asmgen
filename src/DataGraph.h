#pragma once

#include "algorithm.h"

using namespace afk;

enum DGType {
  DG_INPUT,
  DG_OUTPUT,
  DG_MEM_INPUT,
  DG_MEM_OUTPUT,
  DG_BINOP,
  DG_TRINOP,
  DG_CONSTANT
};

class DGNode {
public:
  virtual DGType getType() const = 0;

  virtual std::string toString() const = 0;

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

  virtual std::string toString() const { return name; }

  virtual DGType getType() const { return DG_INPUT; }

  int getLength() const { return length; }
};

class DGConst : public DGNode {
  int value;
  int length;

public:
  DGConst(const int value_, const int length_) : value(value_), length(length_) {}

  virtual std::string toString() const { return std::to_string(value); }

  int getValue() const { return value; }
  int getLength() const { return length; }

  virtual DGType getType() const { return DG_CONSTANT; }
};

class DGMemOut : public DGNode {

  std::string name;
  DGNode* waddr;
  DGNode* wdata;
  int memSize;
  int readSize;

public:

  DGMemOut(const std::string& name_,
           DGNode* const waddr_,
           DGNode* const wdata_,
           const int memSize_,
           const int readSize_) :
    name(name_), waddr(waddr_),
    wdata(wdata_), memSize(memSize_), readSize(readSize_) {}

  DGNode* getWAddr() const { return waddr; }
  DGNode* getWData() const { return wdata; }

  int getMemSize() const { return memSize; }
  int getReadSize() const { return readSize; }

  virtual DGType getType() const { return DG_MEM_OUTPUT; }

  virtual std::string toString() const {
    return name + " " + waddr->toString() + " " +
      wdata->toString() + " " + std::to_string(memSize) + " " +
      std::to_string(readSize);
  }
};

class DGMemIn : public DGNode {

  std::string name;
  DGNode* raddr;
  int memSize;
  int readSize;

public:

  DGMemIn(const std::string& name_,
          DGNode* const raddr_,
          const int memSize_,
          const int readSize_) :
    name(name_),
    raddr(raddr_),
    memSize(memSize_),
    readSize(readSize_) {}

  DGNode* getRAddr() const { return raddr; }

  int getMemSize() const { return memSize; }
  int getReadSize() const { return readSize; }

  virtual DGType getType() const { return DG_MEM_INPUT; }

  virtual std::string toString() const { return "memin"; }
};

class DGOut : public DGNode {
  std::string name;
  int length;
  DGNode* in;

public:

  DGOut(const std::string& name_, const int length_, DGNode* const in_) :
    name(name_), length(length_), in(in_) {}

  DGNode* getInput() const { return in; }

  virtual DGType getType() const { return DG_OUTPUT; }

  void setName(const std::string& nn) { name = nn; }

  int getLength() const { return length; }

  virtual std::string toString() const { return name; }
};

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

  std::string getOpName() const { return op; }

  virtual std::string toString() const {
    return "(" + getOp0()->toString() + " " + op + " " + getOp1()->toString() + ")";
  }
};

class DGTrinop : public DGNode {
  std::string op;
  DGNode* op0;
  DGNode* op1;
  DGNode* op2;
  
public:

  DGTrinop(const std::string& op_,
           DGNode* const op0_,
           DGNode* const op1_,
           DGNode* const op2_) :
    op(op_), op0(op0_), op1(op1_), op2(op2_) {}

  virtual DGType getType() const { return DG_TRINOP; }

  std::string getOpName() const { return op; }

  DGNode* getOp0() const { return op0; }
  DGNode* getOp1() const { return op1; }
  DGNode* getOp2() const { return op2; }

  virtual std::string toString() const {
    return "(" + getOp0()->toString() + " " + op + " " + getOp1()->toString() +
      " " + getOp2()->toString() + ")";
  }

};

DGIn* toInput(DGNode* const node);
DGBinop* toBinop(DGNode* const node);
DGTrinop* toTrinop(DGNode* const node);
DGOut* toOutput(DGNode* const node);
DGMemOut* toMemOutput(DGNode* const node);
DGMemIn* toMemInput(DGNode* const node);

class DataGraph {
protected:
  std::vector<DGNode*> nodes;
  std::map<DGNode*, std::vector<DGNode*> > inEdges;
  std::map<DGNode*, std::vector<DGNode*> > outEdges;
  

public:

  std::vector<DGNode*> getNodes() const { return nodes; }

  std::vector<DGNode*> getOutEdges(DGNode* const n) const {
    return (outEdges.find(n))->second;
  }

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

  DGMemIn* addMemInput(const std::string& name,
                       DGNode* const raddr,
                       const int memSize,
                       const int width) {
    auto dgIn = new DGMemIn(name, raddr, memSize, width);

    insertNode(dgIn);

    return dgIn;
  }

  DGMemOut* addMemOutput(const std::string& name,
                        DGNode* const waddr,
                         DGNode* const wdata,
                        const int memSize,
                        const int width) {
    auto dgIn = new DGMemOut(name, waddr, wdata, memSize, width);

    insertNode(dgIn);

    return dgIn;
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

    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(input));
    map_insert(outEdges, static_cast<DGNode*>(input), static_cast<DGNode*>(dgOut));

    return dgOut;
  }

  DGConst* addConstant(int value, int length) {
    auto dgC = new DGConst(value, length);

    insertNode(dgC);

    return dgC;
  }

  DGTrinop* addTrinop(const std::string& op,
                      DGNode* const op0,
                      DGNode* const op1,
                      DGNode* const op2) {
    auto dgOut = new DGTrinop(op, op0, op1, op2);

    insertNode(dgOut);

    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(op0));
    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(op1));
    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(op2));

    map_insert(outEdges, static_cast<DGNode*>(op0), static_cast<DGNode*>(dgOut));
    map_insert(outEdges, static_cast<DGNode*>(op1), static_cast<DGNode*>(dgOut));
    map_insert(outEdges, static_cast<DGNode*>(op2), static_cast<DGNode*>(dgOut));
    
    return dgOut;

  }
  
  DGBinop* addBinop(const std::string& op,
                    DGNode* const op0,
                    DGNode* const op1) {
    auto dgOut = new DGBinop(op, op0, op1);

    insertNode(dgOut);

    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(op0));
    map_insert(inEdges, static_cast<DGNode*>(dgOut), static_cast<DGNode*>(op1));

    map_insert(outEdges, static_cast<DGNode*>(op0), static_cast<DGNode*>(dgOut));
    map_insert(outEdges, static_cast<DGNode*>(op1), static_cast<DGNode*>(dgOut));
    
    return dgOut;
  }
  
  ~DataGraph() {
    for (auto& nd : nodes) {
      delete nd;
    }
  }
};

std::vector<DGNode*> allInputs(const DataGraph& dg);
