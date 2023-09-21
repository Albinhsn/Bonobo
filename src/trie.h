#include "common.h"
#include "scanner.h"
#include <algorithm>
#include <cctype>
#include <map>

class Node {

  public:
    TokenType type;
    char value;
    std::vector<Node *> children;

    Node(int depth, char value) {
        this->type = TOKEN_IDENTIFIER;
        this->value = value;
        this->children = std::vector<Node *>(26);
        std::fill(this->children.begin(), this->children.end(), nullptr);
    }
};

class Trie {
  private:
    Node *head;
    std::map<std::string, TokenType> INIT_KEYWORDS = {
        {"and", TOKEN_AND},
        {"arr", TOKEN_ARRAY_TYPE},
        {"bool", TOKEN_BOOL_TYPE},
        {"double", TOKEN_DOUBLE_TYPE},
        {"false", TOKEN_FALSE},
        {"for", TOKEN_FOR},
        {"fun", TOKEN_FUN},
        {"else", TOKEN_ELSE},
        {"int", TOKEN_INT_TYPE},
        {"if", TOKEN_IF},
        {"map", TOKEN_MAP_TYPE},
        {"nil", TOKEN_NIL},
        {"or", TOKEN_OR},
        {"print", TOKEN_PRINT},
        {"return", TOKEN_RETURN},
        {"str", TOKEN_STR_TYPE},
        {"struct", TOKEN_STRUCT_TYPE},
        {"true", TOKEN_TRUE},
        {"var", TOKEN_VAR},
        {"while", TOKEN_WHILE}};

    TokenType findNode(std::string keyword, int depth, Node *curr) {
        if (curr == nullptr) {
            return TOKEN_IDENTIFIER;
        }
        if (depth == keyword.size()) {
            return curr->type;
        }

        if ((!isalpha(keyword[depth]) || isupper(keyword[depth]))) {
            return TOKEN_IDENTIFIER;
        }

        int idx = 'z' - keyword[depth++];
        return findNode(keyword, depth, curr->children[idx]);
    }
    void addNode(std::string keyword, TokenType type, int depth, Node *curr) {
        if (depth == keyword.size()) {
            curr->type = type;
            return;
        }

        int idx = 'z' - keyword[depth++];
        if (curr->children[idx] == nullptr) {
            curr->children[idx] = new Node(depth, keyword[depth - 1]);
        }
        curr = curr->children[idx];

        addNode(keyword, type, depth, curr);
    }

    void fillTrie() {
        for (const auto &[key, value] : INIT_KEYWORDS) {
            addNode(key, value, 0, head);
        }
    }

  public:
    TokenType isKeyword(const char *current, int len) {
        std::string keyword(current, len);
        return findNode(keyword, 0, head);
    }

    Trie() {
        this->head = new Node(0, '\0');
        fillTrie();
    }
};
