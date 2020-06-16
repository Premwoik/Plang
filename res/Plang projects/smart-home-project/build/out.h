#include "Arduino.h"
#include "ArrayList.h"
#include "Core.h"
#include "Ethernet.h"
#include "Maybe.h"
#include "MemoryInfo.h"
#include "Message.h"
#include "MessageProcessor.h"
#include "SPI.h"
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
void Void();
void printMemStats();
void Void() {}
int g___otherwise = 0;
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
namespace SmartHomeInitializers {
using namespace Core;
void initOutputs(ArrayList<int> &args___out, uint8_t args___initState);
void initInputs(ArrayList<int> &args___ins);
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
} // namespace SmartHomeInitializers
namespace SmartHomeMessage {}
namespace SmartHomeMessageProcessor {}
namespace SmartHomeMsgCode {}
namespace SmartHomeMsgStatus {}
namespace SmartHomeTimer {
using namespace Core;
using namespace CoreNativeList;
void addTask(nonstd::function<void(int)> args___fun, int args___pin,
             int args___timeout);
void runTasks();
class Task {
public:
  nonstd::function<void(int)> this___fun;
  uint32_t this___time;
  int this___pin;
  Task() {}
  Task(nonstd::function<void(int)> args___fun, int args___pin,
       uint32_t args___time) {
    this___pin = args___pin;
    this___fun = args___fun;
    this___time = args___time;
  }
  void runFun() { this___fun(this___pin); }
  uint32_t getTime() { return this___time; }
};
ArrayList<Task> g___tasks = ArrayList<Task>();
void addTask(nonstd::function<void(int)> args___fun, int args___pin,
             int args___timeout) {
  g___tasks.add(Task(args___fun, args___pin, args___timeout));
}
void runTasks() {
  if (g___tasks.size() == 0) {
    return Void();
  }
  ArrayList<int> finished = ArrayList<int>();
  uint32_t time = millis();
  int i = 0;
  for (Task t : g___tasks) {
    if (t.getTime() < time) {
      t.runFun();
      finished.add(i);
    }
    i = i + 1;
  }
  for (int i : finished) {
    g___tasks.remove(i);
  }
}
} // namespace SmartHomeTimer
namespace Main {
using namespace Core;
using namespace CoreNativeList;
using namespace CoreNativeEthernet;
using namespace SmartHomeMessage;
using namespace SmartHomeMessageProcessor;
using namespace SmartHomeInitializers;
void initEthernet(ArrayList<int> &args___mac);
void setupPins();
int Main();
void loop();
bool stillAlive();
Message processReadMsg(Message &args___msg);
Message digitalWriteCmd(Message &args___msg);
Message setTimeDimmerCmd(Message &args___msg);
EthernetServer g___server = EthernetServer(1000);
uint32_t g___timeout = 60000;
uint32_t g___lastReadMessageTime = 0;
void initEthernet(ArrayList<int> &args___mac) {
  Ethernet.begin(args___mac.getNativePtr());
  Serial.println(Ethernet.localIP());
  g___server.begin();
}
void setupPins() {
  ArrayList<int> highInitList =
      ArrayList<int>(new int[32]{18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
                                 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                                 40, 41, 42, 43, 44, 45, 46, 47, 48, 49},
                     32, 32);
  initOutputs(highInitList, HIGH);
  ArrayList<int> inputsList = ArrayList<int>(new int[1]{2}, 1, 1);
  initInputs(inputsList);
}
int Main() {
  Serial.begin(9600);
  setupPins();
  Serial.println("Hello");
  ArrayList<int> mac = ArrayList<int>(new int[6]{0, 1, 2, 3, 4, 5}, 6, 6);
  initEthernet(mac);
  while (true) {
    SmartHomeTimer::runTasks();
    loop();
  }
  return 0;
}
void loop() {
  Serial.print("123");
  EthernetClient client = g___server.available();
  while (!client) {
    return Void();
  }
  Serial.println("Client has connected!");
  MessageProcessor proc = MessageProcessor(&client);
  g___lastReadMessageTime = millis();
  while ((stillAlive()) && (client.connected())) {
    SmartHomeTimer::runTasks();
    shared_ptr<Message> msg = shared_ptr<Message>(proc.readMessage());
    if (msg.isNotNull()) {
      Message response = processReadMsg(*msg);
      proc.sendMessage(response);
      g___lastReadMessageTime = millis();
      Serial.println("--------");
      printMemStats();
    }
  }
  Serial.println("Client has disconnected.");
  client.stop();
}
bool stillAlive() {
  uint32_t time = millis();
  return (g___lastReadMessageTime + g___timeout) > time;
}
Message processReadMsg(Message &args___msg) {
  if (args___msg.getCode() == Message::SET_HIGH) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == Message::SET_LOW) {
    return digitalWriteCmd(args___msg);
  } else if (args___msg.getCode() == Message::SET_TIME_DIMMER) {
    return setTimeDimmerCmd(args___msg);
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
Message setTimeDimmerCmd(Message &args___msg) {
  Serial.println("SetTimeDimmer message");
  for (int i = 0; i < args___msg.getLength(); i += 3) {
    uint8_t pin = args___msg.getArg(i);
    uint8_t isInverted = args___msg.getArg(i + 1);
    uint32_t timeout = millis() + args___msg.getArg(i + 2) * 100;
    if (isInverted) {
      digitalWrite(pin, HIGH);
      auto fun = [=](int args___x) {
        digitalWrite(args___x, LOW);
        Serial.println("Postponed task dimmer");
        Serial.print(pin);
      };
      SmartHomeTimer::addTask(fun, pin, timeout);
    } else {
      digitalWrite(pin, LOW);
      auto fun = [=](int args___x) { return digitalWrite(args___x, HIGH); };
      SmartHomeTimer::addTask(fun, pin, timeout);
    }
  }
  return Message::okMsg(args___msg.getCode());
}
} // namespace Main
