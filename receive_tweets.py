import tweepy
from tweepy.auth import OAuthHandler
from tweepy import Stream
from tweepy.streaming import Stream
import socket
import json

# request to get credentials at http://apps.twitter.com
consumer_key    = "Jewjf7SdhELNyWseSg3gUESCa"
consumer_secret = "I3iR4lVTPHm6EovExeyRP7Fd6fppXJsk9176gRxO2lnUQ92034"
access_token    = "1403328492079620096-ztfZ5FW5PemqIL2IdakYAio0PaI9l8"
access_secret   = "XxbXxW0LdI1xWH56w84xe2aJyIEe0CJATNEHPZDFZ79Xl"


# we create this class that inherits from the StreamListener in tweepy StreamListener
class TweetsListener(Stream):

    def __init__(self,*args, csocket):
        super().__init__(*args)
        self.client_socket = csocket
    # we override the on_data() function in StreamListener
    def on_data(self, data):
        try:
            message = json.loads( data )
            print( message['text'].encode('utf-8') )
            self.client_socket.send((str(['text']) + "\n").encode('utf-8'))
            return True
        except BaseException as e:
            print("Error on_data: %s" % str(e))
        return True

    def if_error(self, status):
        print(status)
        return True

def send_tweets(c_socket):
    #auth = OAuthHandler(consumer_key, consumer_secret)
    #auth.set_access_token(access_token, access_secret)
    twitter_stream = TweetsListener(consumer_key,consumer_secret,access_token,access_secret,csocket=c_socket)
    twitter_stream.filter(track=['nba']) #we are interested in this topic.


if __name__ == "__main__":
    new_skt = socket.socket()         # initiate a socket object
    host = "127.0.0.1"     # local machine address
    port = 5555                 # specific port for your service.
    new_skt.bind((host, port))        # Binding host and port

    print("Now listening on port: %s" % str(port))

    new_skt.listen(5)                 #  waiting for client connection.
    c, addr = new_skt.accept()        # Establish connection with client. it returns first a socket object,c, and the address bound to the socket

    print("Received request from: " + str(addr))
    # and after accepting the connection, we aill sent the tweets through the socket
    send_tweets(c)
