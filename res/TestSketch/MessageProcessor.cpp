
#include "MessageProcessor.h"

MessageProcessor::MessageProcessor(EthernetClient *client) {
	this->client = client;
}

Message *MessageProcessor::readMessage() {
	static auto MAX_MSG_LEN = 100;

	if (client->available() > 0) {
		Serial.println("Start recaiving message...");
		uint8_t tmp[MAX_MSG_LEN];
		auto isMessage = false;
		uint8_t index = 0;
		while (client->available() > 0 && index < MAX_MSG_LEN) {
			uint8_t readByte = client->read();
			if(isMessage){
				if(readByte == 250)
					return new Message(tmp, index);
				tmp[index] = readByte;
				index++;
			}
			else{
//				wait for start byte else skip
				if(readByte == 255) {
					index = 0;
					isMessage = true;
				}
			}
		}
	}
	return nullptr;
}

void MessageProcessor::sendMessage(Message &message) {
	send(message.toBytes(), message.bytesLength());
}

void MessageProcessor::send(uint8_t *msg, int length) {

	for(int i = 0; i < length; i++){
		Serial.print(msg[i]);
		Serial.print(";");
	}
	Serial.println("END");

	if (client != nullptr)
		client->write(msg, length);
	delete[] msg;
}

