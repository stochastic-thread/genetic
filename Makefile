EC = erlc
EFLAGS = +native

all: *.beam

*.beam: *.erl
	$(EC) $(EFLAGS) $^

clean:
	rm -rf *.beam

.PHONY: clean

