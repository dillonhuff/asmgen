#pragma once

#include "DataGraph.h"

class MemChunk {
public:
  std::string name;
  DGNode* origin;

  MemChunk(const std::string& name_,
           DGNode* const origin_) :
    name(name_), origin(origin_) {}
};

struct RegisterAssignment {
  int maxOffset;
  std::vector<DGNode*> topoOrder;
  std::map<DGNode*, std::string> registerAssignment;

  // Should move to using stack vs. heap offsets, or maybe a unified location
  // type encompassing registers, stack, and heap?
  std::map<MemChunk*, int> offsets;
  std::map<DGNode*, MemChunk*> memLocs;

  RegisterAssignment() : maxOffset(0) {}

  int getMaxOffset() const { return maxOffset; }

  void addOffset(DGNode* const origin, const int offset) {
    auto chk = new MemChunk(origin->toString(), origin);
    offsets.insert({chk, offset});
    memLocs.insert({origin, chk});
    maxOffset += offset;
  }

  void accessChunk(DGNode* const origin, MemChunk* chk) {
    memLocs.insert({origin, chk});
  }

  int getOffset(DGNode* const node) const {
    auto memChunk = memLocs.find(node);

    std::cout << "Searching for " << node->toString() << std::endl;

    assert(memChunk != std::end(memLocs));

    auto& chunk = memChunk->second;

    auto offsetIt = offsets.find(chunk);

    assert(offsetIt != std::end(offsets));

    return offsetIt->second;
  }

  ~RegisterAssignment() {
    for (auto& ofp : offsets) {
      delete ofp.first;
    }
  }
};


static inline std::string layoutStructString(std::map<MemChunk*, int>& offsets) {

  std::vector<std::pair<MemChunk*, int> > sortedOffs;
  for (auto& ofp : offsets) {
    sortedOffs.push_back({ofp.first, ofp.second});
  }

  afk::sort_lt(sortedOffs, [](const std::pair<MemChunk*, int>& l) {
      return l.second;
    });

  std::string decls = "";
  for (auto& ofp : sortedOffs) {
    std::string name = ofp.first->name;
    std::replace(name.begin(), name.end(), '.', '_');
    std::replace(name.begin(), name.end(), ':', '_');
    std::replace(name.begin(), name.end(), ' ', '_');
    std::replace(name.begin(), name.end(), '=', '_');

    std::string typeStr = "<TYPE>";

    auto nd = ofp.first->origin;
    if (nd->getType() == DG_INPUT) {
      auto inNode = toInput(nd);
      typeStr = "uint" + std::to_string(inNode->getLength()) + "_t";

      decls += "\t" + typeStr + " "  + name + ";" + " // Offset = " + std::to_string(ofp.second) + "\n";
    } else if (nd->getType() == DG_OUTPUT) {
      auto inNode = toOutput(nd);
      typeStr = "uint" + std::to_string(inNode->getLength()) + "_t";

      decls += "\t" + typeStr + " "  + name + ";" + " // Offset = " + std::to_string(ofp.second) + "\n";
    } else if (nd->getType() == DG_MEM_INPUT) {
      auto inNode = toMemInput(nd);
      int readSize = inNode->getReadSize();

      decls += "\tuint" + std::to_string(readSize*8) + "_t " + name + " [ " +
        std::to_string(inNode->getMemSize() / readSize) + " ]; " + " // Offset = " + std::to_string(ofp.second) + "\n";

      //decls += "\t" + typeStr + " "  + name + ";" + " // Offset = " + std::to_string(ofp.second) + "\n";
    } else if (nd->getType() == DG_MEM_OUTPUT) {

      auto inNode = toMemOutput(nd);
      int readSize = inNode->getReadSize();

      decls += "\tuint" + std::to_string(readSize*8) + "_t " + name + " [ " +
        std::to_string(inNode->getMemSize() / readSize) + " ]; " + " // Offset = " + std::to_string(ofp.second) + "\n";
      
    } else {
      decls += "\t" + typeStr + " "  + name + ";" + " // Offset = " + std::to_string(ofp.second) + "\n";
    }
    
  }

  return "struct __attribute__((packed)) layout {\n" + decls + "\n};\n";
}

void appendAssignRegisters(DataGraph& dg,
                           RegisterAssignment& asg);

RegisterAssignment assignRegisters(DataGraph& dg);
