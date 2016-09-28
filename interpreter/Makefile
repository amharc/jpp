all: interpreter

interpreter:
	wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
	tar xvvf stack.tar.gz
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-1.1.0-linux-x86_64/stack setup
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-1.1.0-linux-x86_64/stack install --local-bin-path=$(shell pwd)
	mv funlog interpreter
