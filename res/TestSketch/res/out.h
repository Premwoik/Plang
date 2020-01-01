void printMemStats();
int mymax(int a, int b);
int Main();
#include "Arduino.h"
#include "ArrayList.h"
#include "MemoryInfo.h"
#include "unique_ptr.h"
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
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
  int i = 0;
  int top = 5;
  while (i < top) {
    int tak = 30;
    i = i + 1;
    Serial.println(F("Tak czy siak"));
  }
  while (true) {
    printMemStats();
    digitalWrite(13, LOW);
    Serial.println(F("LOW"));
    delay(1000);
    digitalWrite(13, HIGH);
    Serial.println(F("HIGH"));
    delay(500);
    digitalWrite(13, LOW);
    Serial.println("LOW");
    delay(500);
    digitalWrite(13, HIGH);
    Serial.println("HIGH");
    delay(2000);
    int tak = 12;
    Serial.println(mymax(tak, 15));
    unique_ptr<ArrayList<int>> myList(new ArrayList<int>());
    myList->add(10);
    int res = myList->get(0);
    unique_ptr<ArrayList<int>> myList2(
        new ArrayList<int>(new int[3]{1 + 1, 1, 3}, 3, 3));
    Serial.println(res);
  }
  return 0;
}
