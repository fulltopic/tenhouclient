# Tenhou Client for Dp4j rl4j
This is a scala written Tenhou client for Dp4j reinforcement learning module to adjust to its inverse control style.

This client communicate to Tenhou server through TCP connection by text log messages

The client can stand some exception including: connection interruption, invalid action etc.
While it is not good enough as I am not good in this game and my AI seldom won so a lots of errors in good day cases may be missing.
In this case, please just raise issue with log file

## Configuration
* Update tenhouclient/config/ClientSettings and in according to your environment
* Update resources/logback.xml and put into the $CLASSPAH

## Extension
If customized version wanted, please replace tenhouclient/impl/ with your own version
