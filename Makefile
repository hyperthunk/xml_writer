#!/usr/bin/make -f
# Makefile wrapper

all: clean-compile

build:
	@(./rebar compile)

clean:
	@(./rebar clean)

clean-build: clean build

ebin/xml_writer.app: build

test: test-deps ebin/xml_writer.app
	@(./rebar -C test.config eunit -v)

test-deps:
	@(./rebar -C test.config get-deps compile-deps)

.PHONY: all build clean clean-build test
