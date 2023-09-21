#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "trie.h"

static std::string readFile(const char *path) {
    std::ifstream t(path);
    std::stringstream buffer;
    if (t.fail()) {
        std::cout << "file doesn't exist\n";
        exit(1);
    }
    buffer << t.rdbuf();
    t.close();
    return buffer.str();
}

int main(int argc, const char *argv[]) {
    Trie *trie = new Trie();
    printf("%d\n", trie->isKeyword("and", 3) == TOKEN_IDENTIFIER);
    // printf("%d\n", trie->isKeyword("fun", 3) == TOKEN_IDENTIFIER);
    return 0;
    if (argc == 1) {
        printf("Need file name\n");
        exit(1);
    }
    std::string source = readFile(argv[1]);
    compile(source.c_str());
    return 0;
}
