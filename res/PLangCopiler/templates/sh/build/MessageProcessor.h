#ifndef MESSAGE_PROCESSOR_H
#define MESSAGE_PROCESSOR_H

#include <Ethernet.h>
#include "Message.h"

class MessageProcessor {
private:
	EthernetClient *client;


	void send(uint8_t *msg, int length);

public:
	MessageProcessor(EthernetClient *client);

	Message *readMessage();

	void sendMessage(Message & message);
};

#endif

