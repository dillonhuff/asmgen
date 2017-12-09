#include "DataGraph.h"

using namespace std;

DGIn* toInput(DGNode* const node) {
  assert(node->getType() == DG_INPUT);

  return static_cast<DGIn*>(node);
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
