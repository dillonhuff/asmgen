#include "RegisterAssignment.h"

using namespace std;

std::vector<std::string>
nowDeadRegisters(DGNode* op,
                 DataGraph& dg,
                 map<DGNode*, std::string>& regAssignment) {
  if (op->getType() == DG_BINOP) {
    auto* bp = toBinop(op);

    vector<string> freedRegs;
    auto op0 = bp->getOp0();

    if (dg.getOutEdges(op0).size() == 1) {
      freedRegs.push_back(regAssignment[op0]);
    }

    cout << "Freeing " << freedRegs.size() << " regs:";
    for (auto& reg : freedRegs) {
      cout << reg << " ";
    }
    cout<< endl;

    return freedRegs;
  } else if (op->getType() == DG_TRINOP) {
    auto* bp = toTrinop(op);

    vector<string> freedRegs;
    auto op0 = bp->getOp0();
    auto op1 = bp->getOp1();
    auto op2 = bp->getOp2();

    if (dg.getOutEdges(op0).size() == 1) {
      freedRegs.push_back(regAssignment[op0]);
    }

    if (dg.getOutEdges(op1).size() == 1) {
      freedRegs.push_back(regAssignment[op1]);
    }

    if (dg.getOutEdges(op2).size() == 1) {
      freedRegs.push_back(regAssignment[op2]);
    }
    
    cout << "Freeing " << freedRegs.size() << " regs:";
    for (auto& reg : freedRegs) {
      cout << reg << " ";
    }
    cout<< endl;

    return freedRegs;
    
  } else if (op->getType() == DG_OUTPUT) {
    auto* bp = toOutput(op);

    auto op2 = bp->getInput();

    vector<string> freedRegs;
    if (dg.getOutEdges(op2).size() == 1) {
      freedRegs.push_back(regAssignment[op2]);
    }
    
    cout << "Freeing " << freedRegs.size() << " regs:";
    for (auto& reg : freedRegs) {
      cout << reg << " ";
    }
    cout<< endl;

    return freedRegs;
  }

  cout << "None freed" << endl;
  return {};
}

void appendAssignRegisters(DataGraph& dg,
                           RegisterAssignment& asg) {

  auto nodeOrder = topologicalSort(dg);
  afk::concat(asg.topoOrder, nodeOrder);

  int offset = asg.getMaxOffset();
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

        asg.addOffset(node, offset);
        
        offset += toInput(node)->getLength() / 8;
      } else if (node->getType() == DG_OUTPUT) {

        asg.addOffset(node, offset);
        offset += toOutput(node)->getLength() / 8;
      } else if (node->getType() == DG_MEM_INPUT) {

        asg.addOffset(node, offset);
        offset += toMemInput(node)->getMemSize();
      } else if (node->getType() == DG_MEM_OUTPUT) {

        asg.addOffset(node, offset);
        offset += toMemOutput(node)->getMemSize();
      }
    }

  }

  // Horrible hack
  vector<string> x86_32Bit{"%eax", "%ecx", "%edx", "%esi", "%ebx"}; //"esi", "ebx"};
  afk::concat(x86_32Bit, {"%r8d", "%r9d", "%r10d", "%r11d", "%r12d",
        "%r13d", "%r14d", "%r15d"});

  //map<DGNode*, string> regAssignment;
  for (auto& node : nodeOrder) {

    // TODO: Reintroduce when I am done with this little experiment
    //assert(x86_32Bit.size() > 0);

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
        string nextReg = x86_32Bit.back();
        x86_32Bit.pop_back();

        asg.registerAssignment.insert({node, nextReg});
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
      //assert(false);
    }

    afk::concat(x86_32Bit, nowDeadRegisters(node, dg, asg.registerAssignment));
  }

  //asg.registerAssignment = asg.regAssignment;

}

RegisterAssignment assignRegisters(DataGraph& dg) {

  RegisterAssignment asg;

  appendAssignRegisters(dg, asg);

  return asg;

}

