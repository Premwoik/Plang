#include "Arduino.h"
#include "ArrayList.h"
#include "MemoryInfo.h"
#include "unique_ptr.h"
namespace CoreNativeList {}
namespace Core {
void printMemStats();
using namespace CoreNativeList;
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
template <typename T> class ThisTest {
public:
  T a;
  int b = 0;
  ThisTest(T a) {
    this->a = a;
    this->b = 100;
  }
  ThisTest(T a, int b) {
    this->a = a;
    this->b = b;
  }
  T getA() { return a; }
  void setA(T val) { this->a = val; }
};
} // namespace Core
int mymax(int a, int b);
int Main();
using namespace Core;
int mymax(int a, int b) {
  int fuckT12;
  if (a > b) {
    fuckT12 = a;
  } else {
    fuckT12 = b;
  }
  return fuckT12;
}
int Main() {
  Serial.begin(9600);
  pinMode(13, OUTPUT);
  unique_ptr<ArrayList<int>> myList(new ArrayList<int>());
  int i = 0;
  while (true) {
    printMemStats();
    myList->add(10);
    if (i > 100) {
      myList->clear();
      Serial.println(F("Wyczyszczono liste!"));
    }
    unique_ptr<ThisTest<float>> classTest(new ThisTest<float>(13.0, 100));
    float z = classTest->getA();
    Serial.println(z);
    Serial.println(classTest->getA());
  }
  return 0;
}
