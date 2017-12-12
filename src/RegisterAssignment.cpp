#include "RegisterAssignment.h"

using namespace std;

std::vector<std::string>
nowDeadRegisters(set<DGNode*>& allocated,
                 DataGraph& dg,
                 map<DGNode*, std::string>& regAssignment) {

  vector<std::string> freedRegs;
  vector<DGNode*> freedNodes;

  for (auto& node : allocated) {
    bool allOutputsAllocated = true;
    // cout << "Node " << node->toString() << " points to " << endl;

    // for (auto outNode : dg.getOutEdges(node)) {
    //   cout << "\t" << outNode->toString() << endl;
    // }

    for (auto outNode : dg.getOutEdges(node)) {
      
      if (!elem(outNode, allocated)) {
        allOutputsAllocated = false;
        break;
      }
    }

    if (allOutputsAllocated) {
      freedRegs.push_back(regAssignment[node]);
      freedNodes.push_back(node);
    }
  }

  for (auto& node : freedNodes) {
    allocated.erase(node);
  }

  freedRegs = sort_unique(freedRegs);
  cout << "Freeing " << freedRegs.size() << " regs:";
  for (auto& reg : freedRegs) {
    cout << reg << " ";
  }
  cout<< endl;

  return freedRegs;
  
  // if (op->getType() == DG_BINOP) {
  //   auto* bp = toBinop(op);

  //   vector<string> freedRegs;
  //   auto op0 = bp->getOp0();

  //   if (dg.getOutEdges(op0).size() == 1) {
  //     freedRegs.push_back(regAssignment[op0]);
  //   }

  //   cout << "Freeing " << freedRegs.size() << " regs:";
  //   for (auto& reg : freedRegs) {
  //     cout << reg << " ";
  //   }
  //   cout<< endl;

  //   return freedRegs;
  // } else if (op->getType() == DG_TRINOP) {
  //   auto* bp = toTrinop(op);

  //   vector<string> freedRegs;
  //   auto op0 = bp->getOp0();
  //   auto op1 = bp->getOp1();
  //   auto op2 = bp->getOp2();

  //   if (dg.getOutEdges(op0).size() == 1) {
  //     freedRegs.push_back(regAssignment[op0]);
  //   }

  //   if (dg.getOutEdges(op1).size() == 1) {
  //     freedRegs.push_back(regAssignment[op1]);
  //   }

  //   if (dg.getOutEdges(op2).size() == 1) {
  //     freedRegs.push_back(regAssignment[op2]);
  //   }
    
  //   cout << "Freeing " << freedRegs.size() << " regs:";
  //   for (auto& reg : freedRegs) {
  //     cout << reg << " ";
  //   }
  //   cout<< endl;

  //   return freedRegs;
    
  // } else if (op->getType() == DG_OUTPUT) {
  //   auto* bp = toOutput(op);

  //   auto op2 = bp->getInput();

  //   vector<string> freedRegs;
  //   if (dg.getOutEdges(op2).size() == 1) {
  //     freedRegs.push_back(regAssignment[op2]);
  //   }
    
  //   cout << "Freeing " << freedRegs.size() << " regs:";
  //   for (auto& reg : freedRegs) {
  //     cout << reg << " ";
  //   }
  //   cout<< endl;

  //   return freedRegs;
  // }

  // cout << "None freed" << endl;
  // return {};
}

void appendAssignRegisters(DataGraph& dg,
                           RegisterAssignment& asg) {

  auto nodeOrder = topologicalSort(dg);
  afk::concat(asg.topoOrder, nodeOrder);

  for (auto& node : nodeOrder) {

    bool notAdded = true;
    for (auto& ofp : asg.offsets) {
      if (ofp.first->name == node->toString()) {
        notAdded = false;
        asg.accessChunk(node, ofp.first);
        break;
      }
    }

    if (notAdded) {
      if (node->getType() == DG_INPUT) {

        asg.addOffset(node, toInput(node)->getLength() / 8); //offset);
        
        //offset += toInput(node)->getLength() / 8;
      } else if (node->getType() == DG_OUTPUT) {

        asg.addOffset(node, toOutput(node)->getLength() / 8); //offset);
        //offset += toOutput(node)->getLength() / 8;
      } else if (node->getType() == DG_MEM_INPUT) {

        asg.addOffset(node, toMemInput(node)->getMemSize()); //offset);
        //offset += toMemInput(node)->getMemSize();
      } else if (node->getType() == DG_MEM_OUTPUT) {

        asg.addOffset(node, toMemOutput(node)->getMemSize()); //offset);
        //offset += toMemOutput(node)->getMemSize();
      }
    }

  }

  vector<string> allx86_32Bit{"%eax", "%ecx", "%edx", "%esi", "%ebx"}; //"esi", "ebx"};
  afk::concat(allx86_32Bit, {"%r8d", "%r9d", "%r10d", "%r11d", "%r12d",
        "%r13d", "%r14d", "%r15d"});
  
  // Horrible hack
  vector<string> x86_32Bit{"%eax", "%ecx", "%edx", "%esi", "%ebx"}; //"esi", "ebx"};
  afk::concat(x86_32Bit, {"%r8d", "%r9d", "%r10d", "%r11d", "%r12d",
        "%r13d", "%r14d", "%r15d"});

  set<DGNode*> alreadyAllocated;
  for (auto& node : nodeOrder) {

    cout << "Registers: ";
    for (auto& reg : x86_32Bit) {
      cout << reg << ", ";
    }
    cout << endl;

    if (node->getType() == DG_INPUT) {
      if (x86_32Bit.size() > 0) {
        string nextReg = x86_32Bit.back();
        x86_32Bit.pop_back();

        asg.registerAssignment.insert({node, nextReg});
      } else {
        asg.registerAssignment.insert({node, "%NONE"});
      }

    } else if (node->getType() == DG_OUTPUT) {
    } else if (node->getType() == DG_BINOP) {

      DGBinop* bp = static_cast<DGBinop*>(node);
      asg.registerAssignment.insert({node, asg.registerAssignment[bp->getOp1()]});

    } else if (node->getType() == DG_TRINOP) {
      if (x86_32Bit.size() > 0) {
        string nextReg = x86_32Bit.back();
        x86_32Bit.pop_back();

        asg.registerAssignment.insert({node, nextReg});
      } else {
        asg.registerAssignment.insert({node, "%NONE"});
      }


    } else if (node->getType() == DG_CONSTANT) {

      if (x86_32Bit.size() > 0) {

        if (dg.getOutEdges(node).size() == 1) {
          // DGConst* dc = toConstant(node);

          // DGNode* receiver = dg.getOutEdges(node)[0];

          // if (receiver->getType() == DG_BINOP) {

          //   if ((toBinop(receiver)->getOp0() == dc) &&
          //       (toBinop(receiver)->getOp1() != dc)) {
          //     asg.registerAssignment.insert({node, "$" + dc->toString()});
          //   }
          // }

          string nextReg = x86_32Bit.back();
          x86_32Bit.pop_back();

          asg.registerAssignment.insert({node, nextReg});

        } else {
          string nextReg = x86_32Bit.back();
          x86_32Bit.pop_back();

          asg.registerAssignment.insert({node, nextReg});
        }
      } else {
        asg.registerAssignment.insert({node, "%NONE"});
      }

    } else if (node->getType() == DG_MEM_INPUT) {

      if (x86_32Bit.size() > 0) {
        string nextReg = x86_32Bit.back();
        x86_32Bit.pop_back();

        
        asg.registerAssignment.insert({node, nextReg});
      } else {
        asg.registerAssignment.insert({node, "%NONE"});
      }

    } else {
      
      cout << "No register allocation for " << node->toString() << endl;
    }

    alreadyAllocated.insert(node);
    // afk::concat(x86_32Bit, nowDeadRegisters(alreadyAllocated,
    //                                         dg,
    //                                         asg.registerAssignment));

    x86_32Bit = sort_unique(x86_32Bit);
    x86_32Bit = afk::intersection(x86_32Bit, allx86_32Bit);
  }

}

RegisterAssignment assignRegisters(DataGraph& dg) {

  RegisterAssignment asg;

  appendAssignRegisters(dg, asg);

  return asg;

}

