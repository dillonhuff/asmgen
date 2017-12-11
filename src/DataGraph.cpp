#include "DataGraph.h"

using namespace std;

DGIn* toInput(DGNode* const node) {
  assert(node->getType() == DG_INPUT);

  return static_cast<DGIn*>(node);
}

DGConst* toConstant(DGNode* const node) {
  assert(node->getType() == DG_CONSTANT);

  return static_cast<DGConst*>(node);
}

DGMemOut* toMemOutput(DGNode* const node) {
  assert(node->getType() == DG_MEM_OUTPUT);

  return static_cast<DGMemOut*>(node);
}

DGMemIn* toMemInput(DGNode* const node) {
  assert(node->getType() == DG_MEM_INPUT);

  return static_cast<DGMemIn*>(node);
}

DGBinop* toBinop(DGNode* const node) {
  assert(node->getType() == DG_BINOP);

  return static_cast<DGBinop*>(node);
}

DGTrinop* toTrinop(DGNode* const node) {
  assert(node->getType() == DG_TRINOP);

  return static_cast<DGTrinop*>(node);
}

DGOut* toOutput(DGNode* const node) {
  assert(node->getType() == DG_OUTPUT);

  return static_cast<DGOut*>(node);
}

std::vector<DGNode*> allInputs(const DataGraph& dg) {
  vector<DGNode*> nds;
  for (auto& node : dg.getNodes()) {
    if (node->getType() == DG_INPUT) {
      nds.push_back(node);
    }
  }

  return nds;
}

DGNode* maskOp(DGNode* const width, DGNode* const expr, DataGraph& dg) {
  auto cs1 = dg.addConstant(1, 64);
  auto shr = dg.addBinop("<<", cs1, width);
  auto cs2 = dg.addConstant(1, 64);
  auto diff = dg.addBinop("-", shr, cs2);
  auto maskExpr = dg.addBinop("&", diff, expr);

  return maskExpr;
}

std::vector<DGNode*> topologicalSort(DataGraph& dg) {
  vector<DGNode*> nodeOrder;
  vector<DGNode*> ins = allInputs(dg);

  set<DGNode*> alreadyAdded;
  afk::concat(nodeOrder, ins);
  for (auto& node : nodeOrder) {
    alreadyAdded.insert(node);
  }

  vector<DGNode*> nodesLeft;
  for (auto& node : dg.getNodes()) {
    if (!afk::elem(node, alreadyAdded)) {
      nodesLeft.push_back(node);
    }
  }

  while (nodesLeft.size() > 0) {
    cout << "Nodes left size = " << nodesLeft.size() << endl;

    for (auto& node : nodesLeft) {

      assert(node != nullptr);

      cout << "Trying with node ptr = " << node << endl;
      cout << "Node has type        = " << node->getType() << endl;
      cout << "Trying with node     = " << node->toString() << endl;

      bool allInputsAdded = true;
      for (auto& input : dg.getInputs(node)) {
        if (!afk::elem(input, alreadyAdded)) {
          allInputsAdded = false;
          break;
        }
      }

      if (allInputsAdded) {
        alreadyAdded.insert(node);
        nodeOrder.push_back(node);
        afk::remove(node, nodesLeft);
        break;
      }
    }

  }

  return nodeOrder;
}
