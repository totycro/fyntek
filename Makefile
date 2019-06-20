all: watch

watch:
	find src | entr pulp run --no-check-main
