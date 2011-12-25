EC = erlc
EFLAGS = '+native +{hipe,o3}'

all: *.beam

*.beam: *.erl
	$(EC) $(EFLAGS) $^

clean:
	rm -rf *.beam

.PHONY: clean

