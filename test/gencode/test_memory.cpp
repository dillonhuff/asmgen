#include "memory.h"

#include <cassert>
#include <iostream>
#include <bitset>

using namespace std;

void print_memory(const layout& s) {
  for (uint i = 0; i < 3; i++) {
    cout << "m0[ " << i << " ] = " << bitset<16>(s.m0[i]) << endl;
  }
}

int main() {
  layout s;
  s.self_clk = 1;
  s.self_clk_last = 0;
  s.self_write_en = 1;
  s.self_write_addr = 1;
  s.self_write_data = 12;

  s.m0[0] = 0;
  s.m0[1] = 0;
  

  cout << "Before write" << endl;
  print_memory(s);

  memory(&s);

  cout << "After write" << endl;
  print_memory(s);

  cout << "Before assert" << endl;

  assert(s.m0[s.self_write_addr] == s.self_write_data);
  assert(s.m0[0] == 0);

  s.self_write_data = 5;

  memory(&s);

  print_memory(s);

  assert(s.m0[s.self_write_addr] == s.self_write_data);

  cout << "Write without enable" << endl;
  s.self_write_en = 0;
  auto old_write_data = s.self_write_data;

  s.self_write_data = 234;

  memory(&s);

  cout << "After no enable write" << endl;
  print_memory(s);

  assert(s.m0[s.self_write_addr] == old_write_data);
  

  return 0;
}
