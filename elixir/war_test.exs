defmodule WarTest do
  use ExUnit.Case
  doctest War

  test "deal_1" do
    t1 = [1,1,1,1,13,13,13,13,12,12,12,12,11,11,11,11,10,10,10,10,9,9,9,9,8,8,8,8,7,7,7,7,6,6,6,6,5,5,5,5,4,4,4,4,3,3,3,3,2,2,2,2]
    r1=[1, 1, 1, 1, 13, 13, 13, 13, 12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2]
    assert War.deal(t1) == r1
  end

  test "deal_2" do
    t2 = [4,3,2,2,2,2,4,3,4,3,4,3,6,5,6,5,6,5,6,5,8,7,8,7,8,7,8,7,10,9,10,9,10,9,10,9,12,11,12,11,12,11,12,11,1,13,1,13,1,13,1,13]
    r2= [1, 13, 1, 13, 1, 13, 1, 13, 12, 11, 12, 11, 12, 11, 12, 11, 10, 9, 10, 9, 10,
    9, 10, 9, 8, 7, 8, 7, 8, 7, 8, 7, 6, 5, 6, 5, 6, 5, 6, 5, 4, 3, 4, 3, 4, 3, 4,
    3, 2, 2, 2, 2]
    assert War.deal(t2) == r2
  end

  test "deal_3" do
    t3 = [4,3,2,2,2,2,4,3,4,3,4,3,6,5,6,5,6,5,6,5,8,7,8,7,8,7,8,7,10,9,10,9,10,9,10,9,12,11,12,11,12,11,12,11,1,13,1,13,1,13,1,13]
    r3= [1, 13, 1, 13, 1, 13, 1, 13, 12, 11, 12, 11, 12, 11, 12, 11, 10, 9, 10, 9, 10,
    9, 10, 9, 8, 7, 8, 7, 8, 7, 8, 7, 6, 5, 6, 5, 6, 5, 6, 5, 4, 3, 4, 3, 4, 3, 4,
    3, 2, 2, 2, 2]
    assert War.deal(t3) == r3
  end

  test "deal_4" do
    t4 = [1,1,13,12,9,5,11,4,9,3,8,7,7,2,13,10,12,5,10,4,9,6,8,3,1,1,13,12,7,5,11,4,9,3,8,6,7,2,13,10,12,5,11,11,10,8,6,4,6,3,2,2]
    r4= [6, 6, 4, 3, 2, 2, 10, 8, 13, 12, 11, 11, 10, 5, 7, 2, 8, 6, 9, 3, 11, 4, 7, 5,
    13, 12, 1, 1, 9, 8, 6, 3, 10, 4, 12, 5, 13, 10, 7, 2, 8, 7, 9, 3, 11, 4, 9, 5,
    13, 12, 1, 1]
    assert War.deal(t4) == r4
  end

  test "deal_5" do
    t5 = [1,10,13,8,11,9,8,7,11,8,13,7,13,6,12,6,9,5,8,5,7,4,7,4,11,6,12,10,6,3,2,2,12,5,9,3,10,4,9,2,10,3,5,2,1,1,1,13,12,11,4,3]
    r5= [4, 3, 12, 11, 1, 13, 1, 1, 10, 5, 3, 2, 9, 2, 10, 4, 9, 3, 12, 5, 12, 10, 6, 3,
    2, 2, 11, 6, 7, 4, 7, 4, 8, 5, 9, 5, 12, 6, 13, 6, 13, 7, 11, 8, 8, 7, 11, 9,
    13, 8, 1, 10]
    assert War.deal(t5) == r5
  end

end
