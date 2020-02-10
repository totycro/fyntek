export PATH := $(PATH):./node_modules/.bin

all: watch

watch:
	find src | entr pulp run --no-check-main

shell:
	pulp psci

foo:
	pulp bar
