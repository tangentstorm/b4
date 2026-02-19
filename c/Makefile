CC = gcc
CFLAGS = -Wall -Wextra -O2 -std=c11

b4i: b4i.c b4.c b4.h b4ops.h
	$(CC) $(CFLAGS) -o b4i b4i.c b4.c

clean:
	rm -f b4i

.PHONY: clean
