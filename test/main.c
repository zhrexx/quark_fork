#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
struct Slice__char { char* data; size_t size; };
struct str { struct Slice__char slice; };
struct Vec__char { char* data; size_t size; size_t capacity; };
struct Range { size_t start; size_t end; };
struct String { struct Vec__char vector; };

int main();
size_t str__len(struct str);
char* str__data(struct str);
struct String str__to_owned(struct str);
struct Vec__char Vec__char__new();
size_t Vec__char__len(struct Vec__char);
void Vec__char__reserve(struct Vec__char*, size_t);
void Vec__char__push(struct Vec__char*, char);
void Vec__char__push_many(struct Vec__char*, struct Slice__char);
struct Vec__char Vec__char__from(struct Slice__char);
void Vec__char__pop(struct Vec__char*);
struct Range Range__new(size_t, size_t);
size_t Range__len(struct Range);
void write_file(FILE*, struct str);
void print_file(FILE*, struct str);
void print(struct str);
void panic(struct str);
struct String String__new();
struct String String__from(struct str);
struct str String__to_str(struct String);
struct String* String__cat(struct String*, struct str);
char* String__to_cstr(struct String*);


int main() {
}


size_t str__len(struct str self) {
    return ((self . slice) . size);
}


char* str__data(struct str self) {
    return ((self . slice) . data);
}


struct String str__to_owned(struct str self) {
    struct String string;
    (string = String__new());
    Vec__char__push_many((&(string . vector)), (self . slice));
    return string;
}


struct Vec__char Vec__char__new() {
    return (struct Vec__char) { .data = malloc(sizeof(char)), .size = 0, .capacity = 1 };
}


size_t Vec__char__len(struct Vec__char self) {
    return (self . size);
}


void Vec__char__reserve(struct Vec__char* self, size_t n) {
    if((((self -> size) + n) <= (self -> capacity))) 
        return ;
    while(((self -> capacity) < ((self -> size) + n))) 
    {
        ((self -> capacity) = ((self -> capacity) * 2));
    }
    ((self -> data) = realloc((self -> data), (self -> capacity)));
}


void Vec__char__push(struct Vec__char* self, char item) {
    Vec__char__reserve(self, 1);
    ((*((self -> data) + (self -> size))) = item);
    ((self -> size) = ((self -> size) + 1));
}


void Vec__char__push_many(struct Vec__char* self, struct Slice__char slice) {
    size_t i;
    Vec__char__reserve(self, (slice . size));
    (i = 0);
    while((i < (slice . size))) 
    {
        ((*((self -> data) + (self -> size))) = (*((slice . data) + i)));
        ((self -> size) = ((self -> size) + 1));
        (i = (i + 1));
    }
}


struct Vec__char Vec__char__from(struct Slice__char slice) {
    struct Vec__char vec;
    (vec = (struct Vec__char) { .data = malloc((sizeof(char) * (slice . size))), .size = 0, .capacity = (slice . size) });
    Vec__char__push_many((&vec), slice);
    return vec;
}


void Vec__char__pop(struct Vec__char* self) {
    ((self -> size)--);
}


struct Range Range__new(size_t start, size_t end) {
    return (struct Range) { start, end };
}


size_t Range__len(struct Range self) {
    return ((self . end) - (self . start));
}


void write_file(FILE* file, struct str message) {
    fwrite(((message . slice) . data), ((message . slice) . size), sizeof(char), file);
}


void print_file(FILE* file, struct str message) {
    char const newline = '\n';
    fwrite(((message . slice) . data), ((message . slice) . size), sizeof(char), file);
    fwrite((&newline), 1, sizeof(char), file);
}


void print(struct str message) {
    print_file(stdout, message);
}


void panic(struct str message) {
    write_file(stderr, (struct str) { "\x1b[31mpanicked: \x1b[0m", 19 });
    print_file(stderr, message);
    exit(EXIT_FAILURE);
}


struct String String__new() {
    return (struct String) { Vec__char__new() };
}


struct String String__from(struct str string) {
    struct Vec__char vector;
    (vector = Vec__char__new());
    Vec__char__push_many((&vector), (string . slice));
    return (struct String) { vector };
}


struct str String__to_str(struct String self) {
    return (struct str) { (struct Slice__char) { ((self . vector) . data), ((self . vector) . size) } };
}


struct String* String__cat(struct String* self, struct str string) {
    Vec__char__push_many((&(self -> vector)), (string . slice));
    return self;
}


char* String__to_cstr(struct String* self) {
    Vec__char__push((&(self -> vector)), '\0');
    return ((self -> vector) . data);
}
