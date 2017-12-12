#include "conv_3_1.h"

#include <bitset>
#include <iostream>

using namespace std;
int main() {
  layout s;

  s.self_in_0 = 1;
  s.self_clk = 1;
  s.self_clk_last = 0;
  

  s.lb_p4_clamped_stencil_update_stream$mem_2$waddr$reg0 = 0; // Offset = 0
  s.lb_p4_clamped_stencil_update_stream$mem_2$raddr$reg0 = 0; // Offset = 1
  s.lb_p4_clamped_stencil_update_stream$mem_1$waddr$reg0 = 0; // Offset = 2
  s.lb_p4_clamped_stencil_update_stream$mem_1$raddr$reg0 = 0; // Offset = 3

  int nRuns = 20;
  for (int i = 0; i < nRuns; i++) {
    conv_3_1(&s);
  }

  cout << "s.self_out = " << (int) s.self_out << endl;

  return 0;
}
