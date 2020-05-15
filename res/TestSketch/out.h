#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "Ethernet.h"
#include "Maybe.h"
#include "MemoryInfo.h"
#include "Message.h"
#include "MessageProcessor.h"
#include "SPI.h"
#include "timer.h"
#include "unique_ptr.h"
namespace CoreNativeMaybe {}
namespace CoreMyFile {
using namespace CoreNativeMaybe;
String myFileFn();
String myFileFn() { return "ala"; }
} // namespace CoreMyFile
namespace CoreNativeList {
using namespace CoreMyFile;
}
namespace Core {
using namespace CoreNativeList;
void printMemStats();
void printMemStats() {
  float fragmentation = getFragmentation();
  Serial.print(F("Fragmentation level: "));
  Serial.print(fragmentation);
  Serial.println("%");
}
class range {
public:
  int this___start = 0;
  int this___end = 0;
  int this___step = 1;
  range(int args___start, int args___end) {
    this___start = args___start;
    this___end = args___end;
  }
  range(int args___start, int args___end, int args___step) {
    this___start = args___start;
    this___end = args___end;
    this___step = args___step;
  }
  ArrayList<int> toList() {
    int len = (this___end - this___start);
    return ArrayList<int>();
  }
};
} // namespace Core
namespace CoreNativeEthernet {}
namespace CoreNativeTimer {}
namespace SmartHomeMessage {}
namespace SmartHomeMessageProcessor {}
namespace SmartHomeMsgCode {}
namespace SmartHomeMsgStatus {}
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
using namespace SmartHomeMessage;
using namespace SmartHomeMessageProcessor;
using namespace CoreNativeTimer;
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState);
void initInputs(ArrayList<int> &args___ins);
void initEthernet();
int Main();
bool stillAlive();
void loop();
Message processReadMsg(Message &args___msg);
Message digitalWriteCmd(Message &args___msg);
EthernetServer g___server = EthernetServer(1000);
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState) {
  for (int i : args___out) {
    pinMode(i, OUTPUT);
    digitalWrite(i, args___initState);
  }
}
void initInputs(ArrayList<int> &args___ins) {
  for (int i : args___ins) {
    pinMode(i, INPUT);
  }
}
void initEthernet() {
  ArrayList<int> mac = ArrayList<int>(new int[6]{0, 1, 2, 3, 4, 5}, 6, 6);
  Ethernet.begin(mac.getNativePtr());
  Serial.println(Ethernet.localIP());
  g___server.begin();
}
Timer<> g___timer = timer_create_default();
int Main() {
  Serial.begin(9600);
  ArrayList<int> lowInitList = ArrayList<int>(new int[3]{13, 20, 21}, 3, 3);
  initOutputs(lowInitList, LOW);
  ArrayList<int> highInitList = ArrayList<int>(new int[3]{22, 23, 24}, 3, 3);
  initOutputs(highInitList, HIGH);
  g___timer.every(1000, [](void *args___x) { return true; });
  while (true) {
    loop();
    g___timer.tick();
  }
  return 0;
}
int g___timeout = 5 * 60 * 1000;
uint32_t g___lastReadMessageTime = 0;
bool stillAlive() {
  uint32_t time = millis();
  return (g___lastReadMessageTime + g___timeout) > time;
}
void loop() {
  EthernetClient client = g___server.available();
  while (!client) {
    return Serial.println("Client has connected!");
  }
  MessageProcessor proc = MessageProcessor(&client);
  while ((stillAlive()) && (client.connected())) {
    shared_ptr<Message> msg = shared_ptr<Message>(proc.readMessage());
    msg->print();
    if (msg.isNotNull()) {
      Message response = processReadMsg(*msg);
      proc.sendMessage(response);
    }
  }
  Serial.println("Client has disconnected.");
  client.stop();
}
int g___otherwise = 0;
Message processReadMsg(Message &args___msg) {
  if (args___msg.getCode() == Message::SET_HIGH) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == Message::SET_LOW) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == Message::TEST) {
    Serial.println("Test message!");
    return Message::okMsg(Message::TEST);
  } else {
    return Message::errorMsg(args___msg.getCode(), Message::WrongCode);
  }
}
Message digitalWriteCmd(Message &args___msg) {
  Serial.println("DigitalWrite message");
  for (int i = 0; i < args___msg.getLength(); i += 2) {
    digitalWrite(args___msg.getArg(i), args___msg.getArg(i + 1));
  }
  return Message::okMsg(args___msg.getCode());
}
