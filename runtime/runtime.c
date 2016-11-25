#include <stdio.h>
#include <stdlib.h>

struct string {
    int size;
    char data[];
};

typedef struct string string_t;

extern int read() {    
    int d;
    printf ("> ");
    scanf ("%d", &d);
    return d;
}

extern void write(int x) {
    printf ("%d\n", x);
}

extern string_t* strmake(int n, int c) {
    string_t *str = (string_t *) malloc(sizeof(string_t) + n);
    for (int i = 0; i < n; i++) {
        str->data[i] = c;
    }
    str->size = n;
    return str;
}

extern string_t* strset(string_t *s, int i, int c) {
    s->data[i] = c;
    return s;
}

extern int strget(string_t *s, int i) {
    return s->data[i];
}

extern string_t* strdup(string_t *s) {
    string_t *str = (string_t *) malloc(sizeof(string_t) + s->size);
    str->size = s->size;
    for (int i = 0; i < s->size; i++) {
        str->data[i] = s->data[i];
    }
    return str;
}

extern string_t* strcat(string_t *s, string_t *t) {
    string_t *str = (string_t *) malloc(sizeof(string_t) + s->size + t->size);
    str->size = s->size + t->size;
    for (int i = 0; i < s->size; i++) {
        str->data[i] = s->data[i];
    }   
    for (int i = 0; i < t->size; i++) {
        str->data[i + s->size] = t->data[i];
    }
    return str;
}

extern int strcmp(string_t *s, string_t *t) {
    for (int i = 0; i < s->size && i < t->size; i++) {
        if (s->data[i] != t->data[i]) return s->data[i] < t->data[i] ? -1 : 1;
    }
    return s->size == t->size ? 0 : (s->size < t->size) ? -1: 1;
}

extern int strlen(string_t *s) {
    return s->size;
}

extern string_t* strsub(string_t *s, int i, int l) {
    string_t *str = (string_t *) malloc(sizeof(string_t) + l);
    str->size = l;
    for (int j = i; j < i + l; j++)
        str->data[j - i] = s->data[j];
    return str;
}
