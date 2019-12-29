int mymax(int a, int b);
int Main();
#include "Arduino.h"
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
  }
  return 0;
}
