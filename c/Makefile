CC = gcc
CFLAGS = -Wall -Wextra -O2 -std=c11

b4h: b4h.c b4.c b4.h b4ops.h
	$(CC) $(CFLAGS) -o b4h b4h.c b4.c

clean:
	rm -f b4h

.PHONY: clean
