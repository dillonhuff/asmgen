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

  return 0;
}
