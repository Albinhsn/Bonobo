
#include "../src/stack.h"
#include <gtest/gtest.h>
#include <stdexcept>

Value initValue(double nmbr) {
  Value value;
  value.type = VAL_NUMBER;
  value.as.number = nmbr;

  return value;
}

TEST(TestStack, TestStack) {
  Stack *stack = new Stack();

  stack->push(initValue(5.0));

  stack->push(initValue(7.0));

  stack->push(initValue(9.0));
  EXPECT_EQ(stack->length, 3);

  EXPECT_EQ(stack->pop().as.number, 9.0);
  EXPECT_EQ(stack->length, 2);

  stack->push(initValue(11.0));
  EXPECT_EQ(stack->pop().as.number, 11.0);

  EXPECT_EQ(stack->pop().as.number, 7.0);
  EXPECT_EQ(stack->peek().as.number, 5.0);
  EXPECT_EQ(stack->pop().as.number, 5.0);

  stack->push(initValue(69.0));
  EXPECT_EQ(stack->peek().as.number, 69.0);
  EXPECT_EQ(stack->length, 1);
}
