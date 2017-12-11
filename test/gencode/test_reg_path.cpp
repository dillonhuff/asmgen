#include "reg_path.h"

#include <cassert>
#include <iostream>

using namespace std;

int main() {
  layout lt;
  lt.self_in_0 = 1;
  lt.self_in_1 = 3;

  reg_path(&lt);

  assert(lt.self_out_0 == 4);

  cout << "output = " << lt.self_out_0 << endl;

  lt.self_in_0 = 5;
  lt.self_in_1 = 9;
  lt.self_clk = 0;
  lt.self_clk_last = 1;

  reg_path(&lt);

  assert(lt.self_out_0 == 4);

  return 0;
}
