from socket import *
serverIP = 'localhost'
serverPort = 12000
clientSocket = socket(AF_INET, SOCK_STREAM)
sentence = raw_input("Input lowercase sentence:")
clientSocket.connect((serverIP,serverPort))
clientSocket.send(sentence)
modifiedSentence = clientSocket.recv(1024)
print ("From Server:", modifiedSentence)
clientSocket.close()

serverSocket = socket(AF_INET, SOCK_DGRAM)
serverSocket.bind(("", serverPort))
print ("The server is ready to receive")
while 1:
    message, clientAddress = serverSocket.recvfrom(2048)
    modifiedMessage = message.upper()
    serverSocket.sendto(modifiedMessage, clientAddress)

