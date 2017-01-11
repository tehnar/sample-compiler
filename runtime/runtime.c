#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pthread.h"
#define read read_unused
#define write write_unused
#include <unistd.h>
#undef read
#undef write 

#define SIZE(ptr) ((ptr->tag)&((1<<24) - 1))
#define TAG(ptr) ((ptr->tag)>>24)

#define STRING_TAG 0
#define UNBOXED_ARRAY_TAG 1
#define BOXED_ARRAY_TAG 2
#define THREAD_TAG 3

#define SET_SIZE(ptr, size) ptr->tag ^= SIZE(ptr) ^ size
#define SET_TAG(ptr, t) ptr->tag ^= ((ptr->tag>>24)<<24) ^ (t<<24)

#define ASSERT_IS_STRING(s) if (TAG(s) != STRING_TAG) { fprintf(stderr, "Assertion failed, line %d: string expected, but tag %d found\n", __LINE__, TAG(s)); exit(1);}
#define ASSERT_IS_ARRAY(s) if (TAG(s) != BOXED_ARRAY_TAG && TAG(s) != UNBOXED_ARRAY_TAG) { fprintf(stderr, "Assertion failed, line %d: array expected, but tag %d found\n", __LINE__, TAG(s)); exit(1);} 
#define ASSERT_IS_THREAD(s) if (TAG(s) != THREAD_TAG) { fprintf(stderr, "Assertion failed, line %d: thread expected, but tag %d found\n", __LINE__, TAG(s)); exit(1);}

struct string {
    int tag;
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

static string_t* alloc_string(int n) {
    string_t *str = (string_t *) malloc(sizeof(string_t) + n);
    str->tag = 0;
    SET_SIZE(str, n);
    SET_TAG(str, STRING_TAG);
    return str;
}

extern string_t* strmake(int n, int c) {
    string_t *str = alloc_string(n);
    for (int i = 0; i < n; i++) {
        str->data[i] = c;
    }
    return str;
}

extern string_t* strset(string_t *s, int i, int c) {
    ASSERT_IS_STRING(s);
    s->data[i] = c;
    return s;
}

extern int strget(string_t *s, int i) {
    ASSERT_IS_STRING(s);
    return s->data[i];
}

extern string_t* strdup(string_t *s) {
    ASSERT_IS_STRING(s);
    int n = SIZE(s);
    string_t *str = alloc_string(n);
    for (int i = 0; i < n; i++) {
        str->data[i] = s->data[i];
    }
    return str;
}

extern string_t* strcat(string_t *s, string_t *t) {
    ASSERT_IS_STRING(s);
    ASSERT_IS_STRING(t);
    int n = SIZE(s), m = SIZE(t);
    string_t *str = alloc_string(n + m);
    
    for (int i = 0; i < n; i++) {
        str->data[i] = s->data[i];
    }   
    for (int i = 0; i < m; i++) {
        str->data[i + n] = t->data[i];
    }
    return str;
}

extern int strcmp(string_t *s, string_t *t) {
    ASSERT_IS_STRING(s);
    ASSERT_IS_STRING(t);
    int n = SIZE(s), m = SIZE(t);
    for (int i = 0; i < n && i < m; i++) {
        if (s->data[i] != t->data[i]) return s->data[i] < t->data[i] ? -1 : 1;
    }
    return n == m ? 0 : (n < m) ? -1: 1;
}

extern int strlen(string_t *s) {
    ASSERT_IS_STRING(s);
    return SIZE(s);
}

extern string_t* strsub(string_t *s, int i, int l) {
    ASSERT_IS_STRING(s);
    string_t *str = alloc_string(l); 

    for (int j = i; j < i + l; j++)
        str->data[j - i] = s->data[j];
    return str;
}


struct array {
    int tag;
    int data[];
};

typedef struct array array_t;

extern int arrlen(array_t *a) {
    ASSERT_IS_ARRAY(a);
    return SIZE(a);
}

static array_t *__arrmake(int n, int v, int tag) {
    array_t *a = (array_t *) malloc(sizeof(array_t) + n * sizeof(int));
    a->tag = 0;
    SET_SIZE(a, n);
    SET_TAG(a, tag);
    for (int i = 0; i < n; i++) {
        a->data[i] = v;
    }    
    return a;
}

extern array_t *arrmake(int n, int v) {
    return __arrmake(n, v, UNBOXED_ARRAY_TAG);
}

extern array_t *Arrmake(int n, int v) {
    return __arrmake(n, v, BOXED_ARRAY_TAG);
}

extern array_t *arrmake_from_stack(int boxed, int n, ...) {
    va_list stack;
    va_start(stack, n);
    array_t *a = (array_t *) malloc(sizeof(array_t) + n * sizeof(int));
    a->tag = 0;
    SET_SIZE(a, n);
    if (boxed) SET_TAG(a, BOXED_ARRAY_TAG);
    else SET_TAG(a, UNBOXED_ARRAY_TAG);
    for (int i = 0; i < n; i++) {
        a->data[i] = va_arg(stack, int);
    }
    va_end(stack);
    return a;
}

struct thread {
    int tag;
    pthread_t thread;
};

typedef struct thread thread_t;

extern thread_t* thread_create(void *(*func) (void *), void *arg) {
    thread_t *thread = malloc(sizeof(thread_t));
    SET_TAG(thread, THREAD_TAG);
    pthread_create(&(thread->thread), NULL, func, arg);
    return thread;
}

extern int thread_join(thread_t *thread) {
    ASSERT_IS_THREAD(thread);
    pthread_join(thread->thread, NULL);    
    return 0;
}

extern int thread_sleep(int millis) {
    usleep(millis);
    return 0;
}
