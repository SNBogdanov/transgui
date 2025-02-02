

{
  Copyright (C) 2014 Yann Mérignac

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 2.1 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  As a special exception, the copyright holders of this library give
  you permission to link this library with independent modules to
  produce an executable, regardless of the license terms of these
  independent modules,and to copy and distribute the resulting
  executable under terms of your choice, provided that you also meet,
  for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is
  not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the
  library, but you are not obligated to do so. If you do not wish to
  do so, delete this exception statement from your version.

  You should have received a copy of the GNU Lesser General Public
  License along with this library. If not, see
  <http://www.gnu.org/licenses/>.
}

Unit GContnrs;

{$mode objfpc}{$H+}

Interface

Uses Classes, SysUtils;

Const
  MIN_BUCKET_COUNT = 4;
  MAX_BUCKET_COUNT = 1 Shl 30;
  DEFAULT_HASHMAP_LOAD_FACTOR = 1.0;

Type
  EContainerError = Class(Exception);

  { TContainer }
  TContainer = Class
  Protected
    Procedure RaiseContainerEmpty;
    Procedure RaiseCursorDenotesWrongContainer;
    Procedure RaiseCursorIsNil;
    Procedure RaiseError(Const Msg: String);
    Procedure RaiseIndexOutOfRange;
    Procedure RaiseItemAlreadyInSet;
    Procedure RaiseItemNotInSet;
    Procedure RaiseKeyAlreadyInMap;
    Procedure RaiseKeyNotInMap;
    Procedure RaiseMethodNotRedefined;
    Procedure Unused(P: Pointer); Inline;
  End;

  { TGenEnumerator }

  generic TGenEnumerator<_TItem_, _TPosition_> = Class
  Public

    Type
      TGetCurrent = Function(Const Pos: _TPosition_): _TItem_ Of Object;
      TMoveNext = Function(Var Pos: _TPosition_): Boolean Of Object;
  Private
    fGetter: TGetCurrent;
    fMover: TMoveNext;
    fPos: _TPosition_;

    Function GetCurrent: _TItem_;
  Public
    Constructor Create(Const Pos: _TPosition_; Mover: TMoveNext;
      Getter: TGetCurrent);
    Function MoveNext: Boolean;
    Property Current: _TItem_ read GetCurrent;
  End;

  { TAbstractVector }

  TAbstractVector = Class(TContainer)
  Protected
    fCapacity: Integer;
    fSize: Integer;

    Procedure CheckIndex(Index: Integer); Inline;
    Procedure CheckIndexForAdd(Index: Integer); Inline;
    Procedure InsertSpaceFast(Position, Count: Integer); Virtual; Abstract;
    Function ItemToString(Index: Integer): String; Virtual; Abstract;
    Procedure SetCapacity(ACapacity: Integer); Virtual; Abstract;
  Public
    {** Removes all the items from the container. }
    Procedure Clear;

    {** Deletes Count items begining at Position. }
    Procedure Delete(Position: Integer; Count: Integer = 1);

    {** Deletes the first Count items. }
    Procedure DeleteFirst(Count: Integer = 1);

    {** Deletes the last Count items. }
    Procedure DeleteLast(Count: Integer = 1);

    {** Deletes all items in the range [PosFrom..PosTo]. }
    Procedure DeleteRange(PosFrom, PosTo: Integer);

    {** Inserts Count undefined items at Position. }
    Procedure InsertSpace(Position: Integer; Count: Integer = 1);

    {** Returns true if the container is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Copies Count items from Src to Dst. }
    Procedure Move(Src, Dst, Count: Integer); Virtual; Abstract;

    {** If necessary, increases the capacity of the container to ensure that it
      can hold at least MinCapacity items. }
    Procedure Reserve(MinCapacity: Integer);

    {** Resizes the container to contain NewSize items. }
    Procedure Resize(NewSize: Integer);

    {** Reorders the items in reverse order. }
    Procedure Reverse;

    {** Reorders the items in the range [PosFrom..PosTo] in reverse order. }
    Procedure ReverseRange(PosFrom, PosTo: Integer);

    {** Rearrange items randomly. }
    Procedure Shuffle;

    {** Rearrange items in the range [PosFrom..PosTo] randomly. }
    Procedure Shuffle(PosFrom, PosTo: Integer);

    {** Swaps the values of the items designated by I and J. }
    Procedure Swap(I, J: Integer);

    {** Swaps the values of the items designated by I and J (no bounds check). }
    Procedure SwapFast(I, J: Integer); Virtual; Abstract;

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    {** Capacity of the container. }
    Property Capacity: Integer read fCapacity;

    {** Number of items. }
    Property Size: Integer read fSize;
  End;

  { TGenVector }

  generic TGenVector<_TItem_> = Class(TAbstractVector)
  Public

    Type
      PItem = ^_TItem_;
      TCompareItems = Function(Const A, B: _TItem_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TProcessItem = Procedure(Var Item: _TItem_) Of Object;
      TEnumerator = Specialize TGenEnumerator<_TItem_, Integer>;

  Strict  Private
    fItems: Array Of _TItem_;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;

    Function EnumeratorGet(Const Pos: Integer): _TItem_;
    Function EnumeratorNext(Var Pos: Integer): Boolean;
    Procedure Fill(Index, Count: Integer; Const Value: _TItem_);
    Function GetItemFast(Position: Integer): _TItem_; Inline;
    Function GetItemPtrFast(Position: Integer): PItem;
    Procedure InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
    Procedure Quicksort(Left, Right: Integer; Comparator: TCompareItems);
    Class Procedure RealMove(Src, Dst: TGenVector;
      SrcFirst, DstFirst, Count: Integer);
    Procedure SetOnCompareItems(AValue: TCompareItems);
    Procedure SetOnItemToString(AValue: TItemToString);

  Protected
    Procedure InsertSpaceFast(Position, Count: Integer); Override;
    Function ItemToString(Index: Integer): String; Override;
    Procedure SetCapacity(ACapacity: Integer); Override;
  Public
    {** Inserts Count times Item at the end of the container. }
    Procedure Append(Const Item: _TItem_);

    {** Inserts all the items of Src at the end of the container. }
    Procedure AppendAll(Src: TGenVector);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    Procedure AppendRange(Src: TGenVector; PosFrom, PosTo: Integer);



{** Searches for Item using the binary search algorithm. Returns the index of
      Item if its found. Otherwise, returns ( - InsertionPoint - 1 ).
      InsertionPoint is the point at which the key would be inserted into the
      container. }
    Function BinarySearch(Const Item: _TItem_): Integer;
    Function BinarySearch(Const Item: _TItem_; Comparator: TCompareItems): Integer;



{** Searches for Item in range [PosFrom..PosTo] using the binary search
      algorithm. Returns the index of Item if its found. Otherwise, returns
      ( - InsertionPoint - 1 ). InsertionPoint is the point at which the key
      would be inserted into the range. }
    Function BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer): Integer;
    Function BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer;
      Comparator: TCompareItems): Integer;

    {** Returns true if the container contains Item. }
    Function Contains(Const Item: _TItem_): Boolean;
    Function Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;

    {** Creates an empty vector and sets his capacity to InitialCapacity. }
    Constructor Create(InitialCapacity: Integer = 16);

    Function DefaultCompareItems(Const A, B: _TItem_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** If Obj = Self then returns true, else if Obj is not a TGenVector returns
      false, else returns true if Self and Obj contain the sames items. }
    Function Equals(Obj: TObject): Boolean; Override;
    Function Equals(Obj: TObject; Comparator: TCompareItems): Boolean;

    {** Returns the index of the first item equal to Item or -1. }
    Function FindIndex(Const Item: _TItem_): Integer;
    Function FindIndex(Const Item: _TItem_; Comparator: TCompareItems): Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    Function FindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
    Function FindIndex(Const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems): Integer;

    {** Returns the first Item. }
    Function FirstItem: _TItem_; Inline;

    Function GetEnumerator: TEnumerator;

    {** Returns item at position Position. }
    Function GetItem(Position: Integer): _TItem_; Inline;

    {** Returns a pointer designating item at position Position. }
    Function GetItemPtr(Position: Integer): PItem;

    {** Inserts Count times Item before Before. }
    Procedure Insert(Before: Integer; Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src before Before. }
    Procedure InsertAll(Before: Integer; Src: TGenVector);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    Procedure InsertRange(Before: Integer; Src: TGenVector;
      PosFrom, PosTo: Integer);

    {** Returns true if the items are sorted. }
    Function IsSorted: Boolean;
    Function IsSorted(Comparator: TCompareItems): Boolean;

    {** Invokes Process on each items. }
    Procedure Iterate(Process: TProcessItem);

    {** Invokes Process on each items in range [PosFrom..PosTo]. }
    Procedure Iterate(Process: TProcessItem; Const PosFrom, PosTo: Integer);

    {** Returns the last Item. }
    Function LastItem: _TItem_; Inline;

    {** Returns index of the greatest item. }
    Function MaxPos: Integer;
    Function MaxPos(Comparator: TCompareItems): Integer;

    {** Returns index of the greatest item in the range [PosFrom..PosTo]. }
    Function MaxPos(PosFrom, PosTo: Integer): Integer;
    Function MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;



{** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    Procedure Merge(Src: TGenVector);
    Procedure Merge(Src: TGenVector; Comparator: TCompareItems);

    {** Returns index of the lowest item. }
    Function MinPos: Integer;
    Function MinPos(Comparator: TCompareItems): Integer;

    {** Returns index of the lowest item in the range [PosFrom..PosTo]. }
    Function MinPos(PosFrom, PosTo: Integer): Integer;
    Function MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;

    {** Copies Count items from Src to Dst. }
    Procedure Move(Src, Dst, Count: Integer); Override;

    {** Inserts Count times Item at the begining of the container. }
    Procedure Prepend(Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    Procedure PrependAll(Src: TGenVector);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    Procedure PrependRange(Src: TGenVector; PosFrom, PosTo: Integer);

    Procedure ReadFirstItem(out Value: _TItem_); Inline;

    Procedure ReadItem(Position: Integer; out Value: _TItem_);

    Procedure ReadItemFast(Position: Integer; out Value: _TItem_); Inline;

    Procedure ReadLastItem(out Value: _TItem_); Inline;

    {** Replaces items in range [Index..Index + Count - 1] by Value. }
    Procedure Replace(Index, Count: Integer; Const Value: _TItem_);

    {** Returns the index of the first item equal to Item or -1. }
    Function ReverseFindIndex(Const Item: _TItem_): Integer;
    Function ReverseFindIndex(Const Item: _TItem_;
      Comparator: TCompareItems): Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    Function ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
    Function ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems): Integer;

    {** Assigns the value Value to the item at Position. }
    Procedure SetItem(Position: Integer; Const Value: _TItem_); Inline;

    Procedure SetItemFast(Position: Integer; Const Value: _TItem_); Inline;

    {** Sorts the items. }
    Procedure Sort;
    Procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    Procedure Sort(PosFrom, PosTo: Integer);
    Procedure Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);

    {** Swaps the values of the items designated by I and J (no bounds check). }
    Procedure SwapFast(I, J: Integer); Override;

    {** Provides access to the items in the container. }
    Property Items[Index: Integer]: _TItem_ read GetItemFast write SetItemFast; Default;

    {** Provides access to pointers on the items in the container. }
    Property ItemsPtr[Index: Integer]: PItem read GetItemPtrFast;

    Property OnCompareItems: TCompareItems read fOnCompareItems write SetOnCompareItems;

    Property OnItemToString: TItemToString read fOnItemToString write SetOnItemToString;
  End;

  { TGenDeque }

  generic TGenDeque<_TItem_> = Class(TAbstractVector)
  Public

    Type
      PItem = ^_TItem_;
      TCompareItems = Function(Const A, B: _TItem_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TProcessItem = Procedure(Var Item: _TItem_) Of Object;
      TEnumerator = Specialize TGenEnumerator<_TItem_, Integer>;

  Strict  Private
    fItems: Array Of _TItem_;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;
    fStart: Integer;

    Procedure DecRank(Var Rank: Integer); Inline;
    Function Equals(Deque: TGenDeque; Comparator: TCompareItems): Boolean;
    Function EnumeratorGet(Const Pos: Integer): _TItem_;
    Function EnumeratorNext(Var Pos: Integer): Boolean;
    Procedure Fill(Index, Count: Integer; Const Value: _TItem_);
    Function GetItemPtrFast(Position: Integer): PItem;
    Procedure IncRank(Var Rank: Integer); Inline;
    Procedure IncreaseCapacity(ACapacity: Integer);
    Function IndexToRank(Index: Integer): Integer; Inline;
    Procedure InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
    Procedure Quicksort(Left, Right: Integer; Comparator: TCompareItems);
    Function RankToIndex(Rank: Integer): Integer; Inline;
    Class Procedure RealMoveIndex(Src, Dst: TGenDeque;
      SrcFirst, DstFirst, Count: Integer);
    Procedure RealMoveRank(Src, Dst, Count: Integer);
    Procedure ReduceCapacity(ACapacity: Integer);
    Procedure SetOnCompareItems(AValue: TCompareItems);
    Procedure SetOnItemToString(AValue: TItemToString);

  Protected
    Procedure InsertSpaceFast(Position, Count: Integer); Override;
    Function ItemToString(Index: Integer): String; Override;
    Procedure SetCapacity(ACapacity: Integer); Override;
  Public
    {** Inserts Count times Item at the end of the container. }
    Procedure Append(Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the end of the container. }
    Procedure AppendAll(Src: TGenDeque);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    Procedure AppendRange(Src: TGenDeque; PosFrom, PosTo: Integer);



{** Searches for Item using the binary search algorithm. Returns the index of
      Item if its found. Otherwise, returns ( - InsertionPoint - 1 ).
      InsertionPoint is the point at which the key would be inserted into the
      container. }
    Function BinarySearch(Const Item: _TItem_): Integer;
    Function BinarySearch(Const Item: _TItem_; Comparator: TCompareItems): Integer;



{** Searches for Item in range [PosFrom..PosTo] using the binary search
      algorithm. Returns the index of Item if its found. Otherwise, returns
      ( - InsertionPoint - 1 ). InsertionPoint is the point at which the key
      would be inserted into the range. }
    Function BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer): Integer;
    Function BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer;
      Comparator: TCompareItems): Integer;

    {** Returns true if the container contains Item. }
    Function Contains(Const Item: _TItem_): Boolean;
    Function Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;

    {** Creates an empty deque and sets his capacity to InitialCapacity. }
    Constructor Create(InitialCapacity: Integer = 16);

    Function DefaultCompareItems(Const A, B: _TItem_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** If Obj = Self then returns @true, else if Obj is not a TGenDeque returns
      false, else returns @true if Self and Obj contain the sames items. }
    Function Equals(Obj: TObject): Boolean; Override;
    Function Equals(Obj: TObject; Comparator: TCompareItems): Boolean;

    {** Returns the index of the first item equal to Item or -1. }
    Function FindIndex(Const Item: _TItem_): Integer;
    Function FindIndex(Const Item: _TItem_; Comparator: TCompareItems): Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    Function FindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
    Function FindIndex(Const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems): Integer;

    {** Returns the first Item. }
    Function FirstItem: _TItem_; Inline;

    Function GetEnumerator: TEnumerator;

    Function GetItemFast(Position: Integer): _TItem_; Inline;

    {** Returns item at position Position. }
    Function GetItem(Position: Integer): _TItem_; Inline;

    {** Returns a pointer designating item at position Position. }
    Function GetItemPtr(Position: Integer): PItem;

    {** Inserts Count times Item before Before. }
    Procedure Insert(Before: Integer; Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src before Before. }
    Procedure InsertAll(Before: Integer; Src: TGenDeque);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    Procedure InsertRange(Before: Integer; Src: TGenDeque; PosFrom, PosTo: Integer);

    {** Returns true if the items are sorted. }
    Function IsSorted: Boolean;
    Function IsSorted(Comparator: TCompareItems): Boolean;

    {** Invokes Process on each items. }
    Procedure Iterate(Process: TProcessItem);

    {** Invokes Process on each items in range [PosFrom..PosTo]. }
    Procedure Iterate(Process: TProcessItem; Const PosFrom, PosTo: Integer);

    {** Returns the last Item. }
    Function LastItem: _TItem_; Inline;

    {** Returns index of the greatest item. }
    Function MaxPos: Integer;
    Function MaxPos(Comparator: TCompareItems): Integer;

    {** Returns index of the greatest item in the range [PosFrom..PosTo]. }
    Function MaxPos(PosFrom, PosTo: Integer): Integer;
    Function MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;



{** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    Procedure Merge(Src: TGenDeque);
    Procedure Merge(Src: TGenDeque; Comparator: TCompareItems);

    {** Returns index of the lowest item. }
    Function MinPos: Integer;
    Function MinPos(Comparator: TCompareItems): Integer;

    {** Returns index of the lowest item in the range [PosFrom..PosTo]. }
    Function MinPos(PosFrom, PosTo: Integer): Integer;
    Function MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;

    {** Copies Count items from Src to Dst. }
    Procedure Move(Src, Dst, Count: Integer); Override;

    {** Inserts Count times Item at the begining of the container. }
    Procedure Prepend(Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    Procedure PrependAll(Src: TGenDeque);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    Procedure PrependRange(Src: TGenDeque; PosFrom, PosTo: Integer);

    Procedure ReadFirstItem(out Value: _TItem_); Inline;

    Procedure ReadItem(Position: Integer; out Value: _TItem_);

    Procedure ReadItemFast(Position: Integer; out Value: _TItem_); Inline;

    Procedure ReadLastItem(out Value: _TItem_); Inline;

    {** Replaces items in range [Index..Index + Count - 1] by Value. }
    Procedure Replace(Index, Count: Integer; Const Value: _TItem_);

    {** Returns the index of the first item equal to Item or -1. }
    Function ReverseFindIndex(Const Item: _TItem_): Integer;
    Function ReverseFindIndex(Const Item: _TItem_;
      Comparator: TCompareItems): Integer;

    {** Returns a cursor on the first item equal to Item or NilCursor. The search
      starts at the element From.  }
    Function ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
    Function ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer;
      Comparator: TCompareItems): Integer;

    {** Assigns the value Value to the item at Position. }
    Procedure SetItem(Position: Integer; Const Value: _TItem_); Inline;

    Procedure SetItemFast(Position: Integer; Const Value: _TItem_); Inline;

    {** Sorts the items. }
    Procedure Sort;
    Procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    Procedure Sort(PosFrom, PosTo: Integer);
    Procedure Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);

    Procedure SwapFast(I, J: Integer); Override;

    {** Provides access to the items in the container. }
    Property Items[Index: Integer]: _TItem_ read GetItemFast write SetItemFast; Default;

    {** Provides access to pointers on the items in the container. }
    Property ItemsPtr[Index: Integer]: PItem read GetItemPtrFast;

    Property OnCompareItems: TCompareItems read fOnCompareItems write SetOnCompareItems;

    Property OnItemToString: TItemToString read fOnItemToString write SetOnItemToString;
  End;

  TAbstractList = Class;

  { TListCursor }

  TListCursor = Object
  Strict  Private
    fList: TAbstractList;
    fNode: Pointer;

  Public
    {** Check if the cursors designate the same item. }
    Function Equals(Const Cursor: TListCursor): Boolean; Inline;

    {** Check if the cursors designate an item. }
    Function HasItem: Boolean; Inline;

    Constructor Init(AList: TAbstractList; ANode: Pointer);

    {** Returns true if the cursor designates the first element. }
    Function IsFirst: Boolean; Inline;

    {** Returns true if the cursor designates the last element. }
    Function IsLast: Boolean; Inline;

    {** Equivalent to not HasItem. }
    Function IsNil: Boolean; Inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    Procedure MoveNext; Inline;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    Procedure MovePrevious; Inline;

    {** The designated List. }
    Property List: TAbstractList read fList;

    {** The designated node. }
    Property Node: Pointer read fNode write fNode;
  End;

  { TAbstractList }

  TAbstractList = Class(TContainer)
  Protected
    Procedure CheckValid(Const Cursor: TListCursor);
    Procedure CheckNotNil(Const Cursor: TListCursor);
    Function CursorIsFirst(Const Cursor: TListCursor): Boolean; Virtual; Abstract;
    Function CursorIsLast(Const Cursor: TListCursor): Boolean; Virtual; Abstract;
    Procedure CursorMoveNext(Var Cursor: TListCursor); Virtual; Abstract;
    Procedure CursorMovePrev(Var Cursor: TListCursor); Virtual; Abstract;
  End;

  { TGenList }

  generic TGenList<_TItem_> = Class(TAbstractList)
  Public

    Type
      PItem = ^_TItem_;
      TCompareItems = Function(Const A, B: _TItem_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TProcessItem = Procedure(Var Item: _TItem_) Of Object;
      TEnumerator = Specialize TGenEnumerator<_TItem_,
      TListCursor>;

  Strict  Private

    Type
      PNode = ^TNode;

      TNode = Record
        Item: _TItem_;
        Next, Previous: PNode;
      End;

  Strict  Private
    fHead: PNode;
    fOnCompareItems: TCompareItems;
    fOnItemToString: TItemToString;
    fTail: PNode;
    fSize: Integer;
    fNilCursor: TListCursor;

    Procedure DeleteNodesBackward(From: PNode; Count: Integer);
    Procedure DeleteNodesBetween(NodeFrom, NodeTo: PNode);
    Procedure DeleteNodesForward(From: PNode; Count: Integer);
    Function EnumeratorGet(Const Pos: TListCursor): _TItem_;
    Function EnumeratorNext(Var Pos: TListCursor): Boolean;
    Function Equals(List: TGenList; Comparator: TCompareItems): Boolean;
    Function GetItemFast(Const Position: TListCursor): _TItem_; Inline;
    Function GetItemPtrFast(Const Position: TListCursor): PItem; Inline;
    Procedure InsertItem(Const Item: _TItem_; Pos: PNode; Count: Integer);
    Procedure Partition(Pivot, Back: PNode; Comparator: TCompareItems);
    Procedure RealSort(Front, Back: PNode; Comparator: TCompareItems);
    Procedure SetOnCompareItems(AValue: TCompareItems);
    Procedure SetOnItemToString(AValue: TItemToString);
    Procedure SpliceNodes(Before, PosFrom, PosTo: PNode);

  Protected
    Function CursorIsFirst(Const Cursor: TListCursor): Boolean; Override;
    Function CursorIsLast(Const Cursor: TListCursor): Boolean; Override;
    Procedure CursorMoveNext(Var Cursor: TListCursor); Override;
    Procedure CursorMovePrev(Var Cursor: TListCursor); Override;
  Public
    {** Inserts Count times Item at the end of the container. }
    Procedure Append(Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the end of the container. }
    Procedure AppendAll(Src: TGenList);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the end of
      the container. }
    Procedure AppendRange(Src: TGenList; Const PosFrom, PosTo: TListCursor);

    {** Removes all the items from the container. }
    Procedure Clear;

    {** Returns true if the container contains Item. }
    Function Contains(Const Item: _TItem_): Boolean;
    Function Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;

    {** Creates an empty list. }
    Constructor Create;

    Function DefaultCompareItems(Const A, B: _TItem_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;

    {** Deletes Count items begining at Position and then sets Position to
      NilCursor. }
    Procedure Delete(Var Position: TListCursor; Count: Integer = 1);

    {** Deletes the first Count items. }
    Procedure DeleteFirst(Count: Integer = 1);

    {** Deletes the last Count items. }
    Procedure DeleteLast(Count: Integer = 1);

    {** Deletes all items in the range [PosFrom..PosTo]. }
    Procedure DeleteRange(Const PosFrom, PosTo: TListCursor);

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** If Obj = Self then returns true, else if Obj is not a TGenList returns false,
      else returns true if Self and Obj contain the sames items. }
    Function Equals(Obj: TObject): Boolean; Override;
    Function Equals(Obj: TObject; Comparator: TCompareItems): Boolean;

    {** Returns a cursor on the first item equal to Item or NilCursor. }
    Function Find(Const Item: _TItem_): TListCursor;

    Function Find(Const Item: _TItem_; Comparator: TCompareItems): TListCursor;



{** Returns a cursor on the first item equal to Item or NilCursor.The search
      starts at the first element if Position is NilCursor, and at the element
      designated by Position otherwise.  }
    Function Find(Const Item: _TItem_; Const Position: TListCursor): TListCursor;

    Function Find(Const Item: _TItem_; Const Position: TListCursor;
      Comparator: TCompareItems): TListCursor;

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    Function First: TListCursor;

    {** Returns the first Item. }
    Function FirstItem: _TItem_; Inline;

    {** If Index is not in the range [0..Size - 1], then returns NilCursor.
      Otherwise, returns a cursor designating the item at position Index. }
    Function GetCursor(Index: Integer): TListCursor;

    Function GetEnumerator: TEnumerator;

    {** Returns the item designated by Position. }
    Function GetItem(Const Position: TListCursor): _TItem_; Inline;

    {** Returns a pointer designating the item designated by Position. }
    Function GetItemPtr(Const Position: TListCursor): PItem; Inline;

    {** Inserts Count times Item before Before. }
    Procedure Insert(Const Before: TListCursor; Const Item: _TItem_;
      Count: Integer = 1);

    {** Inserts Count times Item before Before. Position designates the first
      newly-inserted element. }
    Procedure Insert(Const Before: TListCursor; Const Item: _TItem_;
      out Position: TListCursor; Count: Integer);

    {** Inserts all the items of Src before Before. }
    Procedure InsertAll(Const Before: TListCursor; Src: TGenList);

    {** Inserts before Before all the items of Src in the range
      [PosFrom..PosTo]. }
    Procedure InsertRange(Const Before: TListCursor; Src: TGenList;
      Const PosFrom, PosTo: TListCursor);

    {** Returns true if the list is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Returns @true if the items are sorted. }
    Function IsSorted: Boolean;

    Function IsSorted(Comparator: TCompareItems): Boolean;

    Procedure Iterate(Process: TProcessItem);

    Procedure Iterate(Process: TProcessItem; Const PosFrom, PosTo: TListCursor);

    {** Returns a cursor that designates the last element of the container or
      NilCursor if the container is empty. }
    Function Last: TListCursor;

    {** Returns the last Item. }
    Function LastItem: _TItem_; Inline;



{** Removes items from Src and inserts them into Self. Afterwards, Self
      contains the union of the items that were initially in Src and Self. Src
      is left empty. If Self and Src are initially sorted, then Self is
      sorted. }
    Procedure Merge(Src: TGenList);
    Procedure Merge(Src: TGenList; Comparator: TCompareItems);

    {** Inserts Count times Item at the begining of the container. }
    Procedure Prepend(Const Item: _TItem_; Count: Integer = 1);

    {** Inserts all the items of Src at the begining of the container. }
    Procedure PrependAll(Src: TGenList);

    {** Inserts all the items of Src in the range [PosFrom..PosTo] at the
      begining of the container. }
    Procedure PrependRange(Src: TGenList; Const PosFrom, PosTo: TListCursor);

    Procedure ReadFirstItem(out Value: _TItem_); Inline;

    Procedure ReadItem(Const Position: TListCursor; out Value: _TItem_);

    Procedure ReadItemFast(Const Position: TListCursor; out Value: _TItem_); Inline;

    Procedure ReadLastItem(out Value: _TItem_); Inline;

    {** Replaces items in range [Position..Position + Count - 1] by Value. }
    Procedure Replace(Const Position: TListCursor; Count: Integer;
      Const Value: _TItem_);

    {** Reorders the items in reverse order. }
    Procedure Reverse;

    {** Returns a cursor on the first item equal to Item or NilCursor. }
    Function ReverseFind(Const Item: _TItem_): TListCursor;
    Function ReverseFind(Const Item: _TItem_;
      Comparator: TCompareItems): TListCursor;



{** Returns a cursor on the first item equal to Item or NilCursor.The search
      starts at the last element if Position is NilCursor, and at the element
      designated by Position otherwise.  }
    Function ReverseFind(Const Item: _TItem_;
      Const Position: TListCursor): TListCursor;
    Function ReverseFind(Const Item: _TItem_; Const Position: TListCursor;
      Comparator: TCompareItems): TListCursor;

    {** Reorders the items in the range [PosFrom..PosTo] in reverse order. }
    Procedure ReverseRange(Const PosFrom, PosTo: TListCursor);

    {** Assigns the value Value to the item designated by Position. }
    Procedure SetItem(Const Position: TListCursor; Const Value: _TItem_);

    Procedure SetItemFast(Const Position: TListCursor; Const Value: _TItem_);
      Inline;

    {** Sorts the items. }
    Procedure Sort;
    Procedure Sort(Comparator: TCompareItems);

    {** Sorts the items in the range [PosFrom..PosTo]. }
    Procedure Sort(Const PosFrom, PosTo: TListCursor);
    Procedure Sort(Const PosFrom, PosTo: TListCursor; Comparator: TCompareItems);

    {** Removes all items of Src and moves them to Self before Before. }
    Procedure Splice(Const Before: TListCursor; Src: TGenList);

    {** Removes from Src the item designated by Position and moves it to Self
      before Before. }
    Procedure Splice(Const Before: TListCursor; Src: TGenList;
      Const Position: TListCursor);

    {** Removes all items of Src in the range [SrcFrom..SrcTo] and moves them to
      Self before Before. }
    Procedure Splice(Const Before: TListCursor; Src: TGenList;
      Const SrcFrom, SrcTo: TListCursor);

    {** Swaps the values of the items designated by I and J. }
    Procedure Swap(Const I, J: TListCursor);

    {** Swaps the nodes designated by I and J. }
    Procedure SwapLinks(Const I, J: TListCursor);

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    {** Provides access to the items in the container. }
    Property Items[Const Index: TListCursor]: _TItem_
      read GetItemFast write SetItemFast; Default;

    {** Provides access to pointers on the items in the container. }
    Property ItemsPtr[Const Index: TListCursor]: PItem read GetItemPtrFast;

    {** A nil cursor. }
    Property NilCursor: TListCursor read fNilCursor;

    Property OnCompareItems: TCompareItems read fOnCompareItems write SetOnCompareItems;

    Property OnItemToString: TItemToString read fOnItemToString write SetOnItemToString;

    {** Number of elements in the list. }
    Property Size: Integer read fSize;
  End;

  { TGenPriorityQueue }

  generic TGenPriorityQueue<_TItem_> = Class(TContainer)
  Public

    Type
      TCompareItems = Function(Const A, B: _TItem_): Integer Of Object;

  Strict  Private
    fCapacity: Integer;
    fItems: Array Of _TItem_;
    fOnCompareItems: TCompareItems;
    fSize: Integer;

    Procedure SetOnCompareItems(AValue: TCompareItems);
    Procedure MoveDown(Index: Integer; Const Item: _TItem_);
    Procedure MoveUp(Index: Integer; Const Item: _TItem_);
  Public
    {** Empty the queue of all items. }
    Procedure Clear;

    {** Creates an empty priority queue. }
    Constructor Create(InitialCapacity: Integer = 16);

    Function DefaultCompareItems(Const A, B: _TItem_): Integer; Virtual;

    {** Returns true if the priority queue is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Frees unused memory. }
    Procedure Pack;

    {** Removes the item from the top of the stack. }
    Procedure Pop;

    {** Adds Item to the top of the stack. }
    Procedure Push(Const Item: _TItem_);

    Procedure ReadTop(out Value: _TItem_);

    {** If necessary, increases the capacity of the container to ensure that it
      can hold at least MinCapacity items. }
    Procedure Reserve(MinCapacity: Integer);

    {** Returns the item at the top of the stack. }
    Function Top: _TItem_;

    {** Capacity of the container. }
    Property Capacity: Integer read fCapacity;

    Property OnCompareItems: TCompareItems read fOnCompareItems write SetOnCompareItems;

    {** Number of elements. }
    Property Size: Integer read fSize;
  End;

  { TGenQueue }

  generic TGenQueue<_TItem_,
  _TContainer_> = Class(TContainer)
  Private
    fData: _TContainer_;
    Function GetSize: Integer; Inline;
  Public
    {** Add the item to the back of the queue. }
    Procedure Append(Const Item: _TItem_);

    {** Empty the queue of all items. }
    Procedure Clear;

    {** Creates an empty queue. }
    Constructor Create;

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** Returns a copy of the item at the front of the queue. }
    Function Front: _TItem_;

    {** Returns true if the queue is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Removes the item from the front of the queue. }
    Procedure Pop;

    Procedure ReadFront(out Value: _TItem_);

    {** Number of items. }
    Property Size: Integer read GetSize;
  End;

  { TGenStack }

  generic TGenStack<_TItem_,
  _TContainer_> = Class(TContainer)
  Private
    fData: _TContainer_;
    Function GetSize: Integer; Inline;
  Public
    {** Removes all the items from the stack. }
    Procedure Clear;

    {** Creates an empty stack. }
    Constructor Create;

    {** Destroys the stack. }
    Destructor Destroy; Override;

    {** Returns true if the stack is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Removes the item from the top of the stack. }
    Procedure Pop;

    {** Adds Item to the top of the stack. }
    Procedure Push(Const Item: _TItem_);

    Procedure ReadTop(out Value: _TItem_);

    {** Returns the item at the top of the stack. }
    Function Top: _TItem_;

    {** Number of items. }
    Property Size: Integer read GetSize;
  End;

  TAbstractHashMap = Class;

  { THashMapCursor }

  THashMapCursor = Object
  Strict  Private
    fBucket: Integer;
    fHashMap: TAbstractHashMap;
    fEntry: Pointer;
    fPrevious: Pointer;

  Public
    {** Check if the cursors designate the same item. }
    Function Equals(Const Cursor: THashMapCursor): Boolean; Inline;

    {** Check if the cursors designate an item. }
    Function HasItem: Boolean; Inline;

    {** Constructor. }
    Constructor Init(HashMap: TAbstractHashMap; BucketNum: Integer;
      AEntry, APrevious: Pointer);

    {** Returns true if the cursor designates the first element. }
    Function IsFirst: Boolean; Inline;

    {** Returns true if the cursor designates the last element. }
    Function IsLast: Boolean; Inline;

    {** Equivalent to not HasItem. }
    Function IsNil: Boolean; Inline;

      {** If cursor is nil then do nothing, else if cursor is last then cursor
        becomes nil cursor, otherwise move cursor to the next item.  }
    Procedure MoveNext; Inline;

    {** Designated bucket. }
    Property Bucket: Integer read fBucket write fBucket;

    Property HashMap: TAbstractHashMap read fHashMap;

    {** Designated entry. }
    Property Entry: Pointer read fEntry write fEntry;

    Property Previous: Pointer read fPrevious write fPrevious;
  End;

  { TAbstractHashMap }

  TAbstractHashMap = Class(TContainer)
  Protected
    Function CursorIsFirst(Const Cursor: THashMapCursor): Boolean;
      Virtual; Abstract;
    Function CursorIsLast(Const Cursor: THashMapCursor): Boolean; Virtual; Abstract;
    Procedure CursorMoveNext(Const Cursor: THashMapCursor); Virtual; Abstract;
  End;

  { TGenHashMap }

  generic TGenHashMap<_TKey_,
  _TItem_> = Class(TAbstractHashMap)
  Public

    Type
      THashKey = Function(Const Key: _TKey_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TKeysEqual = Function(Const A, B: _TKey_): Boolean Of Object;
      TKeyToString = Function(Const Key: _TKey_): String Of Object;
      TEnumerator =
      Specialize TGenEnumerator<_TItem_, THashMapCursor>;

  Private

    Type
      PPEntry = ^PEntry;
      PEntry = ^TEntry;

      TEntry = Record
        Key: _TKey_;
        Value: _TItem_;
        Next: PEntry;
      End;

  Strict  Private
    fBucketCount: Integer;
    fBuckets: PPEntry;
    fFirstNonEmptyBucket: Integer;
    fLastNonEmptyBucket: Integer;
    fMaxBucketCount: Integer;
    fMaxLoadFactor: Real;
    fNilCursor: THashMapCursor;
    fOnHashKey: THashKey;
    fOnItemToString: TItemToString;
    fOnKeysEqual: TKeysEqual;
    fOnKeyToString: TKeyToString;
    fSize: Integer;
    fThreshold: Integer;

    Procedure AppendBuckets(Count: Integer);
    Function CollectEntries: PEntry;

    Procedure DeleteEntry(Bucket: Integer; Entry, Previous: PEntry);
    Procedure DisposeEntries(E: PEntry);
    Function EnumeratorGet(Const Pos: THashMapCursor): _TItem_;
    Function EnumeratorNext(Var Pos: THashMapCursor): Boolean;
    Function FindEntry(Bucket: Integer; Const Key: _TKey_): PEntry;
    Function FindEntry(Bucket: Integer; Const Key: _TKey_;
      out Previous: PEntry): PEntry;
    Function GetEntry(Const Key: _TKey_): PEntry; Inline;
    Function GetEntryAt(Const Position: THashMapCursor): PEntry; Inline;
    Function GetLoadFactor: Real;
    Function IndexFor(Hash: Integer): Integer; Inline;
    Procedure InsertCollectedEntries(CollectedEntries: PEntry);
    Procedure InsertEntry(Bucket: Integer; Entry: PEntry);
    Procedure InsertEntry(Entry, Before: PEntry);
    Function NextNonEmptyBucket(Bucket: Integer): Integer;
    Function NewEntry(Const Key: _TKey_; Const Value: _TItem_): PEntry; Inline;
    Procedure NilifyBuckets(BucketFrom, Count: Integer);
    Function PreviousNonEmptyBucket(Bucket: Integer): Integer;
    Procedure Resize(NewCapacity: Integer);
    Procedure SetOnHashKey(AValue: THashKey);
    Procedure SetOnItemToString(AValue: TItemToString);
    Procedure SetOnKeysEqual(AValue: TKeysEqual);
    Procedure SetOnKeyToString(AValue: TKeyToString);

  Protected
    Function CursorIsFirst(Const Cursor: THashMapCursor): Boolean; Override;
    Function CursorIsLast(Const Cursor: THashMapCursor): Boolean; Override;
    Procedure CursorMoveNext(Const Cursor: THashMapCursor); Override;
  Public
    {** Removes all the items from the container. }
    Procedure Clear;

    {** Returns true if the container contains Key. }
    Function Contains(Const Key: _TKey_): Boolean;

    {** Creates an empty hash map and sets his capacity to InitialCapacity. }
    Constructor Create(InitialCapacity: Integer = MIN_BUCKET_COUNT);

    {** Creates an empty hash map and sets his load factor to MaxLoadFact. }
    Constructor Create(MaxLoadFact: Real);

    {** Creates an empty hash map and sets his capacity to InitialCapacity and
      his load factor to MaxLoadFact. }
    Constructor Create(InitialCapacity: Integer; MaxLoadFact: Real);

    Function DefaultHashKey(Const Key: _TKey_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;
    Function DefaultKeysEqual(Const A, B: _TKey_): Boolean; Virtual;
    Function DefaultKeyToString(Const Key: _TKey_): String; Virtual;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. Otherwise raises an exception. }
    Procedure Delete(Const Key: _TKey_);

    {** Deletes the item designated by Position. }
    Procedure DeleteAt(Const Position: THashMapCursor);

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. }
    Procedure Exclude(Const Key: _TKey_);



{** Checks if an item associated with Key is present. If a match is found,
      a cursor designating the matching item is returned. Otherwise,
      NilCursor is returned. }
    Function Find(Const Key: _TKey_): THashMapCursor;

    {** Returns a cursor that designates the first element of the container
      or NilCursor if the container is empty. }
    Function First: THashMapCursor;

    Function GetEnumerator: TEnumerator;

    Function GetItem(Const Key: _TKey_): _TItem_;

    Function GetItemAt(Const Position: THashMapCursor): _TItem_;

    Function GetKeyAt(Const Position: THashMapCursor): _TKey_;

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then the old value is replaced. }
    Procedure Include(Const Key: _TKey_; Const Value: _TItem_);

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then an exception is raised. }
    Procedure Insert(Const Key: _TKey_; Const Value: _TItem_);



{** If an entry with the key Key is already in the map, then Inserted is set
      to false. Otherwise, Insert inserts Key and Value into the map and sets
      Inserted to true. }
    Procedure Insert(Const Key: _TKey_; Const Value: _TItem_;
      out Inserted: Boolean);

    {** Returns true if the container is empty. }
    Function IsEmpty: Boolean; Inline;

    Procedure ReadItem(Const Key: _TKey_; out Value: _TItem_);
    Procedure ReadItemAt(Const Position: THashMapCursor; out Value: _TItem_);
    Procedure ReadKeyAt(Const Position: THashMapCursor; out Key: _TKey_);



{** Checks if an entry with the key Key is present. If a match is found,
      assigns Key and Value to the matching entry. Otherwise, an exception is
      raised. }
    Procedure Replace(Const Key: _TKey_; Const Value: _TItem_);

    Procedure SetItemAt(Const Position: THashMapCursor; AValue: _TItem_);

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    Property BucketCount: Integer read fBucketCount;

    {** Provides access to the items in the container. }
    Property ItemAt[Const Position: THashMapCursor]: _TItem_
      read GetItemAt write SetItemAt;

    {** Provides access to the items in the container. }
    Property Items[Const Key: _TKey_]: _TItem_ read GetItem write Include; Default;

    {** Provides access to the keys in the container. }
    Property Keys[Const Position: THashMapCursor]: _TKey_ read GetKeyAt;

    Property LoadFactor: Real read GetLoadFactor;

    Property MaxBucketCount: Integer read fMaxBucketCount;

    Property MaxLoadFactor: Real read fMaxLoadFactor;

    {** A nil cursor. }
    Property NilCursor: THashMapCursor read fNilCursor;

    Property OnHashKey: THashKey read fOnHashKey write SetOnHashKey;
    Property OnItemToString: TItemToString read fOnItemToString write SetOnItemToString;
    Property OnKeysEqual: TKeysEqual read fOnKeysEqual write SetOnKeysEqual;
    Property OnKeyToString: TKeyToString read fOnKeyToString write SetOnKeyToString;

    {** Number of items. }
    Property Size: Integer read fSize;
  End;

  TAbstractHashSet = Class(TContainer)
  End;

  { THashSetCursor }

  THashSetCursor = Object
  Strict  Private
    fHashSet: TAbstractHashSet;
    fPos: THashMapCursor;

  Public
    {** Check if the cursors designate the same item. }
    Function Equals(Const Cursor: THashSetCursor): Boolean;

    {** Check if the cursors designate an item. }
    Function HasItem: Boolean; Inline;

    {** Constructor. }
    Constructor Init(HashSet: TAbstractHashSet; Const APos: THashMapCursor);

    {** Returns true if the cursor designates the first element. }
    Function IsFirst: Boolean; Inline;

    {** Returns true if the cursor designates the last element. }
    Function IsLast: Boolean; Inline;

    {** Equivalent to (not HasItem). }
    Function IsNil: Boolean; Inline;

    {** If cursor is nil then do nothing, else if is last then cursor becomes nil
      cursor, otherwise move cursor to the next item.  }
    Procedure MoveNext;

    Property HashSet: TAbstractHashSet read fHashSet;

    {** Designated entry. }
    Property Pos: THashMapCursor read fPos;
  End;

  { TGenHashSet }

  generic TGenHashSet<_TItem_> = Class(TAbstractHashSet)
  Strict  Private

    Type
      TItemEquals = Function(Const A, B: _TItem_): Boolean Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      THashItem = Function(Const Item: _TItem_): Integer Of Object;
      TMap =
      Specialize TGenHashMap<_TItem_, Integer>;
      TEnumerator =
      Specialize TGenEnumerator<_TItem_, THashSetCursor>;

  Strict  Private
    fMap: TMap;
    fNilCursor: THashSetCursor;

    Function EnumeratorGet(Const Pos: THashSetCursor): _TItem_;
    Function EnumeratorNext(Var Pos: THashSetCursor): Boolean;
    Procedure ExchangeContent(ASet: TGenHashSet);
    Function GetItemToString: TItemToString;
    Function GetOnHashItem: THashItem;
    Function GetOnItemsEqual: TItemEquals;
    Function GetSize: Integer; Inline;
    Procedure SetOnHashItem(AValue: THashItem);
    Procedure SetOnItemsEqual(AValue: TItemEquals);
    Procedure SetOnItemToString(AValue: TItemToString);
  Public
    {** Removes all the items from the container. }
    Procedure Clear;

    {** Returns true if the container contains Item. }
    Function Contains(Const Item: _TItem_): Boolean;

    {** Creates an empty hash set and sets his capacity to InitialCapacity. }
    Constructor Create(InitialCapacity: Integer = 16);

    {** Creates an empty hash set and sets his load factor to LoadFact. }
    Constructor Create(LoadFact: Real);

    {** Creates an empty hash set and sets his capacity to InitialCapacity and
      his load factor to LoadFact. }
    Constructor Create(InitialCapacity: Integer; LoadFact: Real);

    Function DefaultItemsEqual(Const A, B: _TItem_): Boolean; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;
    Function DefaultHashItem(Const Item: _TItem_): Integer; Virtual;

    {** Checks if Item is present in the container. If a match is found, removes
      the element from the set. Otherwise, raises an exception. }
    Procedure Delete(Const Item: _TItem_);

    {** Deletes the item designated by Position. }
    Procedure DeleteAt(Const Position: THashSetCursor);

    Destructor Destroy; Override;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right. }
    Procedure Difference(Left, Right: TGenHashSet);

    {** Checks if Item is present in the container. If a match is found, removes
      the item from the set. }
    Procedure Exclude(Const Item: _TItem_);

    {** Excludes all the items of ASet. }
    Procedure ExcludeAll(ASet: TGenHashSet);

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    Function First: THashSetCursor;

    Function GetEnumerator: TEnumerator;

    Function GetItemAt(Const Position: THashSetCursor): _TItem_;

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set.}
    Procedure Include(Const Item: _TItem_);

    {** Includes all the items of ASet. }
    Procedure IncludeAll(ASet: TGenHashSet);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set. Otherwise, raises an exception. }
    Procedure Insert(Const Item: _TItem_);



{** Checks if Item is present in the container. If no match is found, inserts
      the item into the set and sets Inserted to true. Otherwise, sets Inserted
      to false. }
    Procedure Insert(Const Item: _TItem_; out Inserted: Boolean);

    {** Clears Self and then adds to Self all the items of Left that are present
      in Right. }
    Procedure Intersection(Left, Right: TGenHashSet);

    {** Returns true if the container is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Returns true if all the items in Self are present in OfSet. }
    Function IsSubset(OfSet: TGenHashSet): Boolean;

    {** Returns true if at least one item of Self is present in ASet. }
    Function Overlaps(ASet: TGenHashSet): Boolean;

    Procedure ReadItemAt(Const Position: THashSetCursor; out Value: _TItem_);

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right all the items of Right that are not present in Left. }
    Procedure SymmetricDifference(Left, Right: TGenHashSet);

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    {** Clears Self and then adds to Self all the items of Left and all the items
      of Right. }
    Procedure Union(Left, Right: TGenHashSet);

    {** Provides access to the items in the container. }
    Property Items[Const Position: THashSetCursor]: _TItem_ read GetItemAt; Default;

    {** A nil cursor. }
    Property NilCursor: THashSetCursor read fNilCursor;

    Property OnItemsEqual: TItemEquals read GetOnItemsEqual write SetOnItemsEqual;
    Property OnItemToString: TItemToString read GetItemToString write SetOnItemToString;
    Property OnHashItem: THashItem read GetOnHashItem write SetOnHashItem;

    Property Size: Integer read GetSize;
  End;

  TAbstractTreeMap = Class;

  { TTreeMapCursor }

  TTreeMapCursor = Object
  Strict  Private
    fTreeMap: TAbstractTreeMap;
    fEntry: Pointer;

  Public
    {** Check if the cursors designate the same item. }
    Function Equals(Const Cursor: TTreeMapCursor): Boolean; Inline;

    {** Check if the cursors designate an item. }
    Function HasItem: Boolean; Inline;

    {** Constructor. }
    Constructor Init(Map: TAbstractTreeMap; AnEntry: Pointer = nil);

    {** Returns true if the cursor designates the first element. }
    Function IsFirst: Boolean; Inline;

    {** Returns true if the cursor designates the last element. }
    Function IsLast: Boolean; Inline;

    {** Equivalent to (not HasItem). }
    Function IsNil: Boolean; Inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    Procedure MoveNext; Inline;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    Procedure MovePrevious; Inline;

    Property TreeMap: TAbstractTreeMap read fTreeMap;

    {** Designated entry. }
    Property Entry: Pointer read fEntry write fEntry;
  End;

  TAbstractTreeMap = Class(TContainer)
  Protected
    Function CursorIsFirst(Const Cursor: TTreeMapCursor): Boolean;
      Virtual; Abstract;
    Function CursorIsLast(Const Cursor: TTreeMapCursor): Boolean; Virtual; Abstract;
    Procedure CursorMoveNext(Const Cursor: TTreeMapCursor); Virtual; Abstract;
    Procedure CursorMovePrev(Const Cursor: TTreeMapCursor); Virtual; Abstract;
  End;

  { TGenTreeMap }

  generic TGenTreeMap<_TKey_, _TItem_> = Class(TAbstractTreeMap)
  Public

    Type
      TCompareKeys = Function(Const A, B: _TKey_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TKeyToString = Function(Const Key: _TKey_): String Of Object;
      TEnumerator =
      Specialize TGenEnumerator<_TItem_, TTreeMapCursor>;

      PItem = ^_TItem_;

  Private

    Type
      TColor =
      (cBlack, cRed);

      PEntry = ^TEntry;

      TEntry = Record
        Color: TColor;
        Key: _TKey_;
        Left: PEntry;
        Parent: PEntry;
        Right: PEntry;
        Value: _TItem_;
      End;
  Strict  Private
    fNilCursor: TTreeMapCursor;
    fOnCompareKeys: TCompareKeys;
    fOnItemToString: TItemToString;
    fOnKeyToString: TKeyToString;
    fSize: Integer;
    fRoot: PEntry;

    Function ColorOf(E: PEntry): TColor; Inline;
    Procedure DeleteEntry(E: PEntry);
    Procedure DeleteTree(E: PEntry);
    Function EnumeratorGet(Const Pos: TTreeMapCursor): _TItem_;
    Function EnumeratorNext(Var Pos: TTreeMapCursor): Boolean;
    Procedure RepairAfterDelete(E: PEntry);
    Procedure RepairAfterInsert(E: PEntry);
    Function GetCeilingEntry(Const Key: _TKey_): PEntry;
    Function GetEntry(Const Key: _TKey_): PEntry;
    Function GetFirstEntry: PEntry;
    Function GetFloorEntry(Const Key: _TKey_): PEntry;
    Function GetLastEntry: PEntry;
    Function LeftOf(E: PEntry): PEntry; Inline;
    Function NewEntry(AParent: PEntry; Const AKey: _TKey_;
      Const AValue: _TItem_): PEntry;
    Function ParentOf(E: PEntry): PEntry; Inline;
    Function Predecessor(E: PEntry): PEntry;
    Function RightOf(E: PEntry): PEntry; Inline;
    Procedure RotateLeft(E: PEntry);
    Procedure RotateRight(E: PEntry);
    Procedure SetColor(E: PEntry; Color: TColor);
    Procedure SetOnCompareKeys(AValue: TCompareKeys);
    Procedure SetOnItemToString(AValue: TItemToString);
    Procedure SetOnKeyToString(AValue: TKeyToString);
    Function Successor(E: PEntry): PEntry;

  Protected
    Function CursorIsFirst(Const Cursor: TTreeMapCursor): Boolean; Override;
    Function CursorIsLast(Const Cursor: TTreeMapCursor): Boolean; Override;
    Procedure CursorMoveNext(Const Cursor: TTreeMapCursor); Override;
    Procedure CursorMovePrev(Const Cursor: TTreeMapCursor); Override;
  Public


{** Searches for the first entry whose key is not less than Key. If such an
      entry is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    Function Ceiling(Const Key: _TKey_): TTreeMapCursor;

    {** Removes all the items from the container. }
    Procedure Clear;

    {** Returns true if the container contains Key. }
    Function Contains(Const Key: _TKey_): Boolean;

    {** Creates an empty tree map. }
    Constructor Create;

    Function DefaultCompareKeys(Const A, B: _TKey_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;
    Function DefaultKeyToString(Const Key: _TKey_): String; Virtual;

    {** Checks if an item with the key to Key is present. If a match is found,
      removes the item from the map. Otherwise raise an exception. }
    Procedure Delete(Const Key: _TKey_);

    {** Deletes the item designated by Position. }
    Procedure DeleteAt(Const Position: TTreeMapCursor);

    {** Deletes the first item. }
    Procedure DeleteFirst;

    {** Deletes the last item. }
    Procedure DeleteLast;

    {** Destroys the container. }
    Destructor Destroy; Override;

    {** Checks if an item with the key Key is present. If a match is found,
      removes the item from the map. }
    Procedure Exclude(Const Key: _TKey_);



{** Checks if an item associated with Key is present. If a match is found, a
      cursor designating the matching item is returned. Otherwise, NilCursor
      is returned. }
    Function Find(Const Key: _TKey_): TTreeMapCursor;

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    Function First: TTreeMapCursor;

    {** Returns the first Item. }
    Function FirstItem: _TItem_;

    {** If the map is empty raises an exception. Otherwise, returns the smallest
      Key. }
    Function FirstKey: _TKey_;



{** Searches for the last entry whose key is not greater than Key. If such
      an entry is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    Function Floor(Const Key: _TKey_): TTreeMapCursor;

    Function GetEnumerator: TEnumerator;

    Function GetItem(Const Key: _TKey_): _TItem_;

    Function GetItemAt(Const Position: TTreeMapCursor): _TItem_;

    Function GetKeyAt(Const Position: TTreeMapCursor): _TKey_;

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then the old value is replaced. }
    Procedure Include(Const Key: _TKey_; Const Value: _TItem_);

    {** Inserts Key and Value into the map. If an entry with the same Key is
      already in the map, then an exception is raised. }
    Procedure Insert(Const Key: _TKey_; Const Value: _TItem_);



{** If an entry with the same Key is already in the map, then Inserted is
      set to false. Otherwise, Insert inserts Key and Value into the map and
      sets Inserted to true. }
    Procedure Insert(Const Key: _TKey_; Const Value: _TItem_;
      out Inserted: Boolean);

    {** Returns true if the container is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Returns a cursor that designates the last element of the container or
      NilCursor if the container is empty. }
    Function Last: TTreeMapCursor;

    {** Returns the last Item. }
    Function LastItem: _TItem_;

    {** If the map is empty raises an exception. Otherwise, returns the greatest
      Key. }
    Function LastKey: _TKey_;

    Procedure ReadFirstItem(out Value: _TItem_); Inline;

    Procedure ReadFirstKey(out Key: _TKey_); Inline;

    Procedure ReadItem(Const Key: _TKey_; out Value: _TItem_);

    Procedure ReadItemAt(Const Position: TTreeMapCursor; out Value: _TItem_);

    Procedure ReadKeyAt(Const Position: TTreeMapCursor; out Key: _TKey_);

    Procedure ReadLastItem(out Value: _TItem_); Inline;

    Procedure ReadLastKey(out Key: _TKey_); Inline;



{** Checks if an entry with the key Key is present. If a match is found,
      assigns Key and Value to the matching entry. Otherwise, an exception is
      raised. }
    Procedure Replace(Const Key: _TKey_; Const Value: _TItem_);

    Procedure SetItemAt(Const Position: TTreeMapCursor; Value: _TItem_);

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    {** Provides access to the items in the container. }
    Property ItemAt[Const Position: TTreeMapCursor]: _TItem_
      read GetItemAt write SetItemAt;

    {** Provides access to the items in the container. }
    Property Items[Const Key: _TKey_]: _TItem_ read GetItem write Include; Default;

    {** Provides access to the keys in the container. }
    Property Keys[Const Position: TTreeMapCursor]: _TKey_ read GetKeyAt;

    {** A nil cursor. }
    Property NilCursor: TTreeMapCursor read fNilCursor;

    Property OnCompareKeys: TCompareKeys read fOnCompareKeys write SetOnCompareKeys;
    Property OnItemToString: TItemToString read fOnItemToString write SetOnItemToString;
    Property OnKeyToString: TKeyToString read fOnKeyToString write SetOnKeyToString;

    {** Number of items. }
    Property Size: Integer read fSize;
  End;

  TAbstractTreeSet = Class(TContainer)
  End;

  TTreeSetCursor = Object
  Strict  Private
    fTreeSet: TAbstractTreeSet;
    fPos: TTreeMapCursor;

  Public
    {** Check if the cursors designate the same item. }
    Function Equals(Const Cursor: TTreeSetCursor): Boolean;

    {** Check if the cursors designate an item. }
    Function HasItem: Boolean; Inline;

    {** Constructor. }
    Constructor Init(TreeSet: TAbstractTreeSet; Const APos: TTreeMapCursor);

    {** Returns true if the cursor designates the first element. }
    Function IsFirst: Boolean; Inline;

    {** Returns true if the cursor designates the last element. }
    Function IsLast: Boolean; Inline;

    {** Equivalent to (not HasItem). }
    Function IsNil: Boolean; Inline;

    {** If cursor is nil then do nothing, else if cursor is last then cursor
      becomes nil cursor, otherwise move cursor to the next item.  }
    Procedure MoveNext;

    {** If cursor is nil then do nothing, else if cursor is first then cursor
      becomes nil cursor, otherwise move cursor to the previous item.  }
    Procedure MovePrevious;

    Property TreeSet: TAbstractTreeSet read fTreeSet;

    {** Designated entry. }
    Property Pos: TTreeMapCursor read fPos;
  End;

  { TGenTreeSet }

  generic TGenTreeSet<_TItem_> = Class(TAbstractTreeSet)
  Public

    Type
      TCompareItems = Function(Const A, B: _TItem_): Integer Of Object;
      TItemToString = Function(Const Item: _TItem_): String Of Object;
      TEnumerator = Specialize TGenEnumerator<_TItem_, TTreeSetCursor>;

  Private


    Type

      TMap = Specialize TGenTreeMap<_TItem_, Integer>;

  Private

    fMap: TMap;
    fNilCursor: TTreeSetCursor;

    Function EnumeratorGet(Const Pos: TTreeSetCursor): _TItem_;
    Function EnumeratorNext(Var Pos: TTreeSetCursor): Boolean;
    Procedure ExchangeContent(ASet: TGenTreeSet);
    Function GetOnCompareItems: TCompareItems;
    Function GetOnItemToString: TItemToString;
    Function GetSize: Integer; Inline;
    Procedure SetOnCompareItems(AValue: TCompareItems);
    Procedure SetOnItemToString(AValue: TItemToString);
  Public


{** Searches for the first item which is not less than Item. If such an item
      is found, a cursor that designates it is returned. Otherwise NilCursor is
      returned. }
    Function Ceiling(Const Item: _TItem_): TTreeSetCursor;

    {** Removes all the items from the container. }
    Procedure Clear;

    {** Returns true if the container contains Item. }
    Function Contains(Const Item: _TItem_): Boolean;

    {** Creates an empty tree set. }
    Constructor Create;

    Function DefaultCompareItems(Const A, B: _TItem_): Integer; Virtual;
    Function DefaultItemToString(Const Item: _TItem_): String; Virtual;

    {** Checks if Item is present in the container. If a match is found, removes
      the element from the set. Otherwise, raises an exception. }
    Procedure Delete(Const Item: _TItem_);

    {** Deletes the item designated by Position. }
    Procedure DeleteAt(Const Position: TTreeSetCursor);

    {** Deletes the first item. }
    Procedure DeleteFirst;

    {** Deletes the last item. }
    Procedure DeleteLast;

    Destructor Destroy; Override;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right. }
    Procedure Difference(Left, Right: TGenTreeSet);

    {** Checks if Item is present in the container. If a match is found, removes
      the item from the set. }
    Procedure Exclude(Const Item: _TItem_);

    {** Excludes all the items of ASet. }
    Procedure ExcludeAll(ASet: TGenTreeSet);

    {** Returns a cursor that designates the first element of the container or
      NilCursor if the container is empty. }
    Function First: TTreeSetCursor;

    {** Returns the first Item. }
    Function FirstItem: _TItem_;



{** Searches for the last item which is not greater than Item. If such an
      item is found, a cursor that designates it is returned. Otherwise
      NilCursor is returned. }
    Function Floor(Const Item: _TItem_): TTreeSetCursor;

    Function GetEnumerator: TEnumerator;

    Function GetItemAt(Const Position: TTreeSetCursor): _TItem_;

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set.}
    Procedure Include(Const Item: _TItem_);

    {** Includes all the items of ASet. }
    Procedure IncludeAll(ASet: TGenTreeSet);

    {** Checks if Item is present in the container. If no match is found, inserts
      the item into the set. Otherwise, raises an exception. }
    Procedure Insert(Const Item: _TItem_);



{** Checks if Item is present in the container. If no match is found, inserts
      the item into the set and sets Inserted to true. Otherwise, sets Inserted
      to false. }
    Procedure Insert(Const Item: _TItem_; out Inserted: Boolean);

    {** Clears Self and then adds to Self all the items of Left that are present
      in Right. }
    Procedure Intersection(Left, Right: TGenTreeSet);

    {** Returns true if the set is empty. }
    Function IsEmpty: Boolean; Inline;

    {** Returns true if all the items in Self are present in OfSet. }
    Function IsSubset(OfSet: TGenTreeSet): Boolean;

    {** Returns a cursor that designates the last element of the container
      or NilCursor if the container is empty. }
    Function Last: TTreeSetCursor;

    {** Returns the last Item. }
    Function LastItem: _TItem_;

    {** Returns true if at least one item of Self is present in ASet. }
    Function Overlaps(ASet: TGenTreeSet): Boolean;

    Procedure ReadFirstItem(out Value: _TItem_); Inline;

    Procedure ReadItemAt(Const Position: TTreeSetCursor; out Value: _TItem_);

    Procedure ReadLastItem(out Value: _TItem_); Inline;

    {** Clears Self and then adds to Self all the items of Left that are not
      present in Right all the items of Right that are not present in Left. }
    Procedure SymmetricDifference(Left, Right: TGenTreeSet);

    {** Return a string representation for the container. }
    Function ToString: String; Override;

    {** Clears Self and then adds to Self all the items of Left and all the
      items of Right. }
    Procedure Union(Left, Right: TGenTreeSet);

    {** Provides access to the items in the container. }
    Property Items[Const Position: TTreeSetCursor]: _TItem_ read GetItemAt; Default;

    {** A nil cursor. }
    Property NilCursor: TTreeSetCursor read fNilCursor;

    Property OnCompareItems: TCompareItems read GetOnCompareItems
      write SetOnCompareItems;

    Property OnItemToString: TItemToString read GetOnItemToString
      write SetOnItemToString;

    {** Number of items. }
    Property Size: Integer read GetSize;
  End;


  { TBitSet }

  {** Class to store collections of bits. }

  TBitSet = Class(TContainer)
  Private

    fBits: Array Of Byte;
    fExtraMask: Byte;
    fLen: Integer;
    fSize: Integer;

    Procedure ClearExtraBits;
  Public
    {** Performs a logical AND on the bits in the bitset with the bits of
      BitSet. }
    Procedure AndBits(BitSet: TBitSet);

    {** Returns @true if all the bits of the bitset are set, and @false
      otherwise. }
    Function All: Boolean;

    {** Returns @true if any of the bits in the bitset is set, and @false
      otherwise. }
    Function Any: Boolean;

    {** Returns the number of bits in the bitset that are set. }
    Function Cardinality: Integer;

    {** Sets to @false the bit at position Index.}
    Procedure Clear(Index: Integer);

    {** Sets to @false all bits in the bitset.}
    Procedure ClearAll;

    Constructor Create(Size: Integer);

    Constructor Create(Size: Integer; Value: QWord);

    Constructor Create(Size: Integer; Const Value: String);

    Constructor Create(Value: TBitSet);

    Constructor Create(Size: Integer; Value: TBitSet);

    Procedure Debug;

    {** Returns @true only if the bitset and Value are of the same size and have
      exactly the same set of bits set to @true.}
    Function Equals(Obj: TObject): Boolean; Override;

    {** Flips the bit at position Index. }
    Procedure Flip(Index: Integer);

    {** Flips all bits in the bitset. }
    Procedure FlipAll;

    Procedure FlipFast(Index: Integer); Inline;

    Procedure Initialize(Value: Int64);

    Procedure Initialize(Const Value: String);

    Procedure Initialize(Value: TBitSet);

    {** Returns @true if none of the bits in the bitset is set to @true, and
      @false otherwise.}
    Function None: Boolean;

    Procedure NotBits(BitSet: TBitSet);

    {** Performs a logical OR on the bits in the bitset with the bits of
      BitSet. }
    Procedure OrBits(BitSet: TBitSet);

    {** Sets to @true all bits in the bitset.}
    Procedure SetAll;

    {** Sets to Value the bit at position Index.}
    Procedure SetBit(Index: Integer; Value: Boolean);

    Procedure SetBitFast(Index: Integer; Value: Boolean); Inline;

    {** Sets to @true the bit at position Index.}
    Procedure SetOn(Index: Integer);

    Procedure ShiftLeft(Count: Integer = 1);

    Procedure ShiftRight(Count: Integer = 1);

    {** Returns @true if the bit at position Index is set, and @false
      otherwise.}
    Function Test(Index: Integer): Boolean;

    Function TestFast(Index: Integer): Boolean; Inline;

    Function ToInteger: Integer;

    Function ToInt64: Int64;

    Function ToQWord: QWord;

    Function ToString: String; Override;

    {** Performs a logical XOR on the bits in the bitset with the bits of
      BitSet. }
    Procedure XorBits(BitSet: TBitSet);

    Property Bits[Index: Integer]: Boolean read TestFast write SetBitFast; Default;

    Property Size: Integer read fSize;
  End;

Function HashData(Data: Pbyte; DataSize: Integer): Integer;
Function HashString(Const Str: String): Integer;

Implementation

Uses
  Math;

Const
  S_BitSetsAreIncompatible =
    'bit sets are incompatible';
  S_ContainerEmpty =
    'container is empty';
  S_CursorIsNil =
    'cursor is nil';
  S_CursorDenotesWrongContainer =
    'cursor denotes wrong container';
  S_IndexOutOfRange =
    'index out of range';
  S_InvalidBitSetSize =
    'invalid bit set size';
  S_InvalidBinaryValue =
    'invalid binary value';
  S_ItemNotInSet =
    'item not in set';
  S_ItemAlreadyInSet =
    'item already in set';
  S_KeyNotInMap =
    'key not in map';
  S_KeyAlreadyInMap =
    'key already in map';
  S_MethodNotRedefined =
    'method not redefined';

  SBox: Array [Byte] Of Longword = ($F53E1837, $5F14C86B, $9EE3964C,
    $FA796D53,
    $32223FC3, $4D82BC98,
    $A0C7FA62,
    $63E2C982, $24994A5B,
    $1ECE7BEE,
    $292B38EF,
    $D5CD4E56, $514F4303,
    $7BE12B83,
    $7192F195, $82DC7300,
    $084380B4,
    $480B55D3,
    $5F430471, $13F75991,
    $3F9CF22C,
    $2FE0907A, $FD8E1E69,
    $7B1D5DE8,
    $D575A85C,
    $AD01C50A, $7EE00737,
    $3CE981E8,
    $0E447EFA, $23089DD6,
    $B59F149F,
    $13600EC7,
    $E802C8E6, $670921E4,
    $7207EFF0,
    $E74761B0, $69035234,
    $BFA40F19,
    $F63651A0,
    $29E64C26, $1F98CCA7,
    $D957007E,
    $E71DDC75, $3E729595,
    $7580B7CC,
    $D7FAF60B,
    $92484323, $A44113EB,
    $E4CBDE08,
    $346827C9, $3CF32AFA,
    $0B29BCF1,
    $6E29F7DF,
    $B01E71CB, $3BFBC0D1,
    $62EDC5B8,
    $B7DE789A, $A4748EC9,
    $E17A4C4F,
    $67E5BD03,
    $F3B33D1A, $97D8D3E9,
    $09121BC0,
    $347B2D2C, $79A1913C,
    $504172DE,
    $7F1F8483,
    $13AC3CF6, $7A2094DB,
    $C778FA12,
    $ADF7469F, $21786B7B,
    $71A445D0,
    $A8896C1B,
    $656F62FB, $83A059B3,
    $972DFE6E,
    $4122000C, $97D9DA19,
    $17D5947B,
    $B1AFFD0C,
    $6EF83B97, $AF7F780B,
    $4613138A,
    $7C3E73A6, $CF15E03D,
    $41576322,
    $672DF292,
    $B658588D, $33EBEFA9,
    $938CBF06,
    $06B67381, $07F192C6,
    $2BDA5855,
    $348EE0E8,
    $19DBB6E3, $3222184B,
    $B69D5DBA,
    $7E760B88, $AF4D8154,
    $007A51AD,
    $35112500,
    $C9CD2D7D, $4F4FB761,
    $694772E3,
    $694C8351, $4A7E3AF5,
    $67D65CE1,
    $9287DE92,
    $2518DB3C, $8CB4EC06,
    $D154D38F,
    $E19A26BB, $295EE439,
    $C50A1104,
    $2153C6A7,
    $82366656, $0713BC2F,
    $6462215A,
    $21D9BFCE, $BA8EACE6,
    $AE2DF4C1,
    $2A8D5E80,
    $3F7E52D1, $29359399,
    $FEA1D19C,
    $18879313, $455AFA81,
    $FADFE838,
    $62609838,
    $D1028839, $0736E92F,
    $3BCA22A3,
    $1485B08A, $2DA7900B,
    $852C156D,
    $E8F24803,
    $00078472, $13F0D332,
    $2ACFD0CF,
    $5F747F5C, $87BB1E2F,
    $A7EFCB63,
    $23F432F0,
    $E6CE7C5C, $1F954EF6,
    $B609C91B,
    $3B4571BF, $EED17DC0,
    $E556CDA0,
    $A7846A8D,
    $FF105F94, $52B7CCDE,
    $0E33E801,
    $664455EA, $F2C70414,
    $73E7B486,
    $8F830661,
    $8B59E826, $BB8AEDCA,
    $F3D70AB9,
    $D739F2B9, $4A04C34A,
    $88D0F089,
    $E02191A2,
    $D89D9C78, $192C2749,
    $FC43A78F,
    $0AAC88CB, $9438D42D,
    $9E280F7A,
    $36063802,
    $38E8D018, $1C42A9CB,
    $92AAFF6C,
    $A24820C5, $007F077F,
    $CE5BC543,
    $69668D58,
    $10D6FF74, $BE00F621,
    $21300BBE,
    $2E9E8F46, $5ACEA629,
    $FA1F86C7,
    $52F206B8,
    $3EDF1A75, $6DA8D843,
    $CF719928,
    $73E3891F, $B4B95DD6,
    $B2A42D27,
    $EDA20BBF,
    $1A58DBDF, $A449AD03,
    $6DDEF22B,
    $900531E6, $3D3BFF35,
    $5B24ABA2,
    $472B3E4C,
    $387F2D75, $4D8DBA36,
    $71CB5641,
    $E3473F3F, $F6CD4B7F,
    $BF7D1428,
    $344B64D0,
    $C5CDFCB6, $FE2E0182,
    $2C37A673,
    $DE4EB7A3, $63FDC933,
    $01DC4063,
    $611F3571,
    $D167BFAF, $4496596F,
    $3DEE0689,
    $D8704910, $7052A114,
    $068C9EC5,
    $75D0E766,
    $4D54CC20, $B44ECDE2,
    $4ABC653E,
    $2C550A21, $1A52C0DB,
    $CFED03D0,
    $119BAFE2,
    $876A6133, $BC232088,
    $435BA1B2,
    $AE99BBFA, $BB4F08E4,
    $A62B5F49,
    $1DA4B695,
    $336B84DE, $DC813D31,
    $00C134FB,
    $397A98E6, $151F0E64,
    $D9EB3E69,
    $D3C7DF60,
    $D2F2C336, $2DDD067B,
    $BD122835,
    $B0B3BD3A, $B0D54E46,
    $8641F1E4,
    $A0B38F96,
    $51D39199, $37A6AD75,
    $DF84EE41,
    $3C034CBA, $ACDA62FC,
    $11923B8B,
    $45EF170A);

  Card: Array [Byte] Of Byte = (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2
    ,
    3, 3, 4,
    1,
    2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3
    ,
    4, 4, 5, 1, 2, 2, 3, 2, 3, 3, 4, 2,
    3,
    3, 4, 3, 4, 4, 5, 2, 3, 3, 4, 3, 4
    ,
    4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 1, 2,
    2,
    3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4
    ,
    5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4,
    5,
    4, 5, 5, 6, 2, 3, 3, 4, 3, 4, 4, 5
    ,
    3, 4, 4, 5, 4, 5, 5, 6, 3, 4, 4, 5,
    4,
    5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 1
    ,
    2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3,
    4,
    4, 5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4
    ,
    4, 5, 4, 5, 5, 6, 2, 3, 3, 4, 3, 4,
    4,
    5, 3, 4, 4, 5, 4, 5, 5, 6, 3, 4, 4
    ,
    5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6,
    7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5
    ,
    4, 5, 5, 6, 3, 4, 4, 5, 4, 5, 5, 6,
    4,
    5, 5, 6, 5, 6, 6, 7, 3, 4, 4, 5, 4
    ,
    5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 4,
    5,
    5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7
    ,
    7, 8);

  {--- HashData ---}
  {$PUSH}
{$O-}{$R-}{$Q-}
Function HashData(Data: Pbyte; DataSize: Integer): Integer;
Var

  I: Integer;
Begin
  Result := 0;
  For
    I := 1 To DataSize Do
  Begin
    Result :=
      Result Xor Integer(SBox[Data^]);
    Result :=
      Result * 3;
    Inc(
      Data);
  End;
End;
{$POP}

{--- HashString ---}
Function HashString(Const Str: String): Integer;
Begin
  If Str = '' Then
    Result := 0
  Else
    Result :=
      HashData(@Str[1], Length(Str));
End;

{===============}
{=== TBitSet ===}
{===============}

{$push}
{$rangechecks off}
{$overflowchecks off}

{--- TBitSet.ClearExtraBits ---}
Procedure TBitSet.ClearExtraBits;
Begin
  If fExtraMask <> High(Byte) Then
    fBits
      [fLen - 1] := fBits[fLen - 1] And fExtraMask;
End;

{--- TBitSet.AndBits ---}
Procedure TBitSet.AndBits(BitSet: TBitSet);
Var

  I: Integer;
Begin
  If BitSet.fSize <> fSize Then
    RaiseError(
      S_BitSetsAreIncompatible);

  For
    I := 0 To fLen - 1 Do
    fBits
      [I] := fBits[I] And BitSet.fBits[I];

  ClearExtraBits;
End;

{--- TBitSet.All ---}
Function TBitSet.All: Boolean;
Var

  I: Integer;
Begin
  For
    I := 0 To fLen - 2 Do
    If fBits[I] <> High(Byte) Then
    Begin
      Result :=
        False;
      Exit;
    End;

  Result :=
    (fBits[fLen - 1] = fExtraMask);
End;

{--- TBitSet.Any ---}
Function TBitSet.Any: Boolean;
Var

  I: Integer;
Begin
  Result := False;
  For
    I := 0 To fLen - 1 Do
    If fBits[I] <> 0 Then
    Begin
      Result := True;
      Break;
    End;
End;

{--- TBitSet.Cardinality ---}
Function TBitSet.Cardinality: Integer;
Var

  I: Integer;
Begin
  Result := 0;

  For
    I := 0 To fLen - 2 Do
    Result :=
      Result + Card[fBits[I]];
  Result :=
    Result + Card[(fBits[fLen - 1] And fExtraMask)];
End;

{--- TBitSet.Clear ---}
Procedure TBitSet.Clear(Index: Integer);
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;
  SetBitFast(
    Index, False);
End;

{--- TBitSet.ClearAll ---}
Procedure TBitSet.ClearAll;
Var

  I: Integer;
Begin
  For
    I := Low(fBits) To High(fBits) Do
    fBits
      [I] := 0;
End;

{--- TBitSet.Create ---}
Constructor TBitSet.Create(Size: Integer);
Var

  ArraySize, I: Integer;
Begin
  If Size <= 0 Then
    RaiseError(
      S_InvalidBitSetSize);

  fSize := Size;
  fLen :=
    (fSize + (SizeOf(Byte) * 8) - 1) Div (SizeOf(Byte) * 8);

  SetLength(
    fBits, fLen);

  ArraySize :=
    fLen * SizeOf(Byte) * 8;
  If ArraySize = Size Then
    fExtraMask :=
      High(Byte)
  Else
  Begin
    fExtraMask := 1;
    For
      I := 2 To SizeOf(Byte) * 8 - (ArraySize - Size) Do
      fExtraMask :=
        (fExtraMask Shl 1) Or 1;
  End;
End;

{--- TBitSet.Create ---}
Constructor TBitSet.Create(Size: Integer; Value: QWord);
Begin
  Create(Size);
  Initialize(
    Value);
End;

{--- TBitSet.Create ---}
Constructor TBitSet.Create(Size: Integer; Const Value: String);
Begin
  Create(Size);
  Initialize(
    Value);
End;

{--- TBitSet.Create ---}
Constructor TBitSet.Create(Value: TBitSet);
Begin
  Create(
    Value.fSize, Value);
End;

{--- TBitSet.Create ---}
Constructor TBitSet.Create(Size: Integer; Value: TBitSet);
Var

  I, IMax: Integer;
Begin
  Create(Size);

  IMax :=
    Min(Size - 1, Value.fSize - 1);
  For
    I := 0 To IMax Do
    If Value.TestFast(I) Then
      SetBitFast(
        I, True);
End;

{--- TBitSet.Debug ---}
Procedure TBitSet.Debug;
Var

  I: Integer;
Begin
  Write(
    'TBitSet@', HexStr(Self), ' : fLen=', fLen, ' fSize=', fSize);
  WriteLn(
    ' fExtraMask=', BinStr(fExtraMask, SizeOf(fExtraMask) * 8));
  Write(
    'fBits=[');
  For
    I := Low(fBits) To High(fBits) Do
    Write(
      fBits[I], ' ');
  WriteLn(']');
End;

{--- TBitSet.Equals ---}
Function TBitSet.Equals(Obj: TObject): Boolean;
Var

  I: Integer;
  Value: TBitSet;
Begin
  If Obj Is TBitSet Then
  Begin
    Value :=
      Obj As TBitSet;
    If fSize <> Value.fSize Then
      RaiseError(
        S_BitSetsAreIncompatible);

    Result := True;
    For
      I := Low(fBits) To High(fBits) Do
      If fBits[I] <> Value.fBits[I] Then
      Begin
        Result :=
          False;
        Exit;
      End;
  End
  Else
    Result :=
      False;
End;

{--- TBitSet.Flip ---}
Procedure TBitSet.Flip(Index: Integer);
Var

  Rank, NBit: Integer;
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;

  Rank :=
    Index Div (SizeOf(Byte) * 8);
  NBit :=
    Index Mod (SizeOf(Byte) * 8);

  fBits
    [Rank] := fBits[Rank] Xor (Byte(1) Shl NBit);
End;

{--- TBitSet.FlipAll ---}
Procedure TBitSet.FlipAll;
Var

  I: Integer;
Begin
  For
    I := Low(fBits) To High(fBits) Do
    fBits
      [I] := Not fBits[I];
  ClearExtraBits;
End;

{--- TBitSet.FlipFast ---}
Procedure TBitSet.FlipFast(Index: Integer);
Var

  Rank, NBit: Integer;
Begin
  Rank :=
    Index Div (SizeOf(Byte) * 8);
  NBit :=
    Index Mod (SizeOf(Byte) * 8);

  fBits
    [Rank] := fBits[Rank] Xor (Byte(1) Shl NBit);
End;

{--- TBitSet.Initialize ---}
Procedure TBitSet.Initialize(Value: Int64);
Const

  NBits =
    SizeOf(Int64) * 8;
Var

  I, IMax: Integer;
Begin
  ClearAll;

  IMax :=
    Min(NBits - 1, fSize - 1);
  For
    I := 0 To IMax Do
  Begin
    If (Value And 1) <> 0 Then
      SetBitFast(
        I, True);
    Value :=
      Value Shr 1;
  End;
End;

{--- TBitSet.Initialize ---}
Procedure TBitSet.Initialize(Const Value: String);
Var

  I, IMax, Len: Integer;
Begin
  ClearAll;

  Len :=
    Length(Value);
  IMax :=
    Min(Len, fSize);
  For
    I := 1 To IMax Do
  Begin
    If Value[I] = '1' Then
      SetBitFast(
        IMax - I, True)
    Else
      If Value[I] <> '0' Then
        RaiseError(
          S_InvalidBinaryValue);
  End;
End;

{--- TBitSet.Initialize ---}
Procedure TBitSet.Initialize(Value: TBitSet);
Var

  I, IMax: Integer;
Begin
  ClearAll;
  IMax :=
    Min(fSize - 1, Value.fSize - 1);
  For
    I := 0 To IMax Do
    SetBitFast(
      I, Value.TestFast(I));
End;

{--- TBitSet.None ---}
Function TBitSet.None: Boolean;
Begin
  Result := Not Any;
End;

{--- TBitSet.NotBits ---}
Procedure TBitSet.NotBits(BitSet: TBitSet);
Var

  I: Integer;
  B: Integer;
Begin
  If BitSet.fSize <> fSize Then
    RaiseError(
      S_BitSetsAreIncompatible);

  For
    I := 0 To fLen - 1 Do
  Begin
    B := fBits[I];
    fBits
      [I] := B And (B Xor BitSet.fBits[I]);
  End;

  ClearExtraBits;
End;

{--- TBitSet.OrBits ---}
Procedure TBitSet.OrBits(BitSet: TBitSet);
Var

  I: Integer;
Begin
  If BitSet.fSize <> fSize Then
    RaiseError(
      S_BitSetsAreIncompatible);

  For
    I := 0 To fLen - 1 Do
    fBits
      [I] := fBits[I] Or BitSet.fBits[I];

  ClearExtraBits;
End;

{--- TBitSet.SetAll ---}
Procedure TBitSet.SetAll;
Var

  I: Integer;
Begin
  For
    I := Low(fBits) To High(fBits) Do
    fBits
      [I] := High(Byte);
  ClearExtraBits;
End;

{--- TBitSet.SetBit ---}
Procedure TBitSet.SetBit(Index: Integer; Value: Boolean);
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;
  SetBitFast(
    Index, Value);
End;

{--- TBitSet.SetBitFast ---}
Procedure TBitSet.SetBitFast(Index: Integer; Value: Boolean);
Var

  Rank, NBit: Integer;
  Mask: Byte;
Begin
  Rank :=
    Index Div (SizeOf(Byte) * 8);
  NBit :=
    Index Mod (SizeOf(Byte) * 8);

  Mask := 1 Shl NBit;

  If Value Then
    fBits
      [Rank] := fBits[Rank] Or Mask
  Else
  Begin
    Mask := Not Mask;
    fBits
      [Rank] := fBits[Rank] And Mask;
  End;

End;

{--- TBitSet.SetOn ---}
Procedure TBitSet.SetOn(Index: Integer);
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;
  SetBitFast(
    Index, True);
End;

{--- TBitSet.ShiftLeft ---}
Procedure TBitSet.ShiftLeft(Count: Integer);
Var

  I: Integer;
Begin
  If Count = 0 Then
    Exit
  Else
    If Count < 0 Then
      ShiftRight(-Count)
    Else
      If Count >= fSize Then
        ClearAll
      Else
        If Count Mod 8 = 0 Then
        Begin
          Count :=
            Count Div 8;

          For
            I := fLen - Count - 1 Downto 0 Do
            fBits
              [I + Count] := fBits[I];

          For
            I := 0 To Count - 1 Do
            fBits
              [I] := 0;

          ClearExtraBits;
        End
        Else
        Begin
          For
            I := fSize - Count - 1 Downto 0 Do
            SetBitFast(
              I + Count, TestFast(I));

          For
            I := 0 To Count - 1 Do
            SetBitFast(
              I, False);
        End;
End;

{--- TBitSet.ShiftRight ---}
Procedure TBitSet.ShiftRight(Count: Integer);
Var

  I: Integer;
Begin
  If Count = 0 Then
    Exit
  Else
    If Count < 0 Then
      ShiftLeft(-Count)
    Else
      If Count >= fSize Then
        ClearAll
      Else
        If Count Mod 8 = 0 Then
        Begin
          Count :=
            Count Div 8;

          For
            I := Count To fLen - 1 Do
            fBits
              [I - Count] := fBits[I];

          For
            I := fLen - 1 Downto fLen - Count Do
            fBits
              [I] := 0;

          ClearExtraBits;
        End
        Else
        Begin
          For
            I := Count To fSize - 1 Do
            SetBitFast(
              I - Count, TestFast(I));

          For
            I := fSize - Count To fSize - 1 Do
            SetBitFast(
              I, False);
        End;
End;

{--- TBitSet.Test ---}
Function TBitSet.Test(Index: Integer): Boolean;
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;
  Result :=
    TestFast(Index);
End;

{--- TBitSet.TestFast ---}
Function TBitSet.TestFast(Index: Integer): Boolean;
Var

  Rank, NBit: Integer;
Begin
  Rank :=
    Index Div (SizeOf(Byte) * 8);
  NBit :=
    Index Mod (SizeOf(Byte) * 8);

  Result :=
    (fBits[Rank] And (1 Shl NBit)) <> 0;
End;

{--- TBitSet.ToInteger ---}
Function TBitSet.ToInteger: Integer;
Var

  I, IMax: Integer;
Begin
  Result := 0;

  IMax :=
    Min(fSize - 1, SizeOf(Integer) * 8 - 1);
  For
    I := IMax Downto 0 Do
  Begin
    Result :=
      Result Shl 1;
    If TestFast(I) Then
      Result :=
        Result Or 1;
  End;
End;

{--- TBitSet.ToInt64 ---}
Function TBitSet.ToInt64: Int64;
Begin
  Result :=
    Int64(ToQWord);
End;

{--- TBitSet.ToQWord ---}
Function TBitSet.ToQWord: QWord;
Var

  I, IMax: Integer;
Begin
  Result := 0;

  IMax :=
    Min(fSize - 1, SizeOf(QWord) * 8 - 1);
  For
    I := IMax Downto 0 Do
  Begin
    Result :=
      Result Shl 1;
    If TestFast(I) Then
      Result :=
        Result Or 1;
  End;
End;

{--- TBitSet.ToString ---}
Function TBitSet.ToString: String;
Var

  Bit: Char;
  I: Integer;
Begin
  SetLength(
    Result, fSize);

  For
    I := 0 To fSize - 1 Do
  Begin
    If TestFast(I) Then
      Bit := '1'
    Else
      Bit := '0';

    Result
      [fSize - I] := Bit;
  End;
End;

{--- TBitSet.XorBits ---}
Procedure TBitSet.XorBits(BitSet: TBitSet);
Var

  I: Integer;
Begin
  If BitSet.fSize <> fSize Then
    RaiseError(
      S_BitSetsAreIncompatible);

  For
    I := 0 To fLen - 1 Do
    fBits
      [I] := fBits[I] Xor BitSet.fBits[I];

  ClearExtraBits;
End;

{$pop}

{======================}
{=== TGenEnumerator ===}
{======================}

{--- TGenEnumerator.GetCurrent ---}
Function TGenEnumerator.GetCurrent: _TItem_;
Begin
  Result :=
    fGetter(fPos);
End;

{--- TGenEnumerator.Create ---}
Constructor TGenEnumerator.Create(Const Pos: _TPosition_; Mover: TMoveNext;
  Getter: TGetCurrent);
Begin
  fPos := Pos;
  fMover := Mover;
  fGetter :=
    Getter;
End;

{--- TGenEnumerator.MoveNext ---}
Function TGenEnumerator.MoveNext: Boolean;
Begin
  Result :=
    fMover(fPos);
End;

{==================}
{=== TContainer ===}
{==================}

{--- TContainer.RaiseContainerEmpty ---}
Procedure TContainer.RaiseContainerEmpty;
Begin
  Raise
  EContainerError.Create(S_ContainerEmpty);
End;

{--- TContainer.RaiseCursorDenotesWrongContainer ---}
Procedure TContainer.RaiseCursorDenotesWrongContainer;
Begin
  Raise
  EContainerError.Create(S_CursorDenotesWrongContainer);
End;

{--- TContainer.RaiseCursorIsNil ---}
Procedure TContainer.RaiseCursorIsNil;
Begin
  Raise
  EContainerError.Create(S_CursorIsNil);
End;

{--- TContainer.RaiseError ---}
Procedure TContainer.RaiseError(Const Msg: String);
Begin
  Raise
  EContainerError.Create(Msg);
End;

{--- TContainer.RaiseIndexOutOfRange ---}
Procedure TContainer.RaiseIndexOutOfRange;
Begin
  Raise
  EContainerError.Create(S_IndexOutOfRange);
End;

{--- TContainer.RaiseItemAlreadyInSet ---}
Procedure TContainer.RaiseItemAlreadyInSet;
Begin
  Raise
  EContainerError.Create(S_ItemAlreadyInSet);
End;

{--- TContainer.RaiseItemNotInSet ---}
Procedure TContainer.RaiseItemNotInSet;
Begin
  Raise
  EContainerError.Create(S_ItemNotInSet);
End;

{--- TContainer.RaiseKeyAlreadyInMap ---}
Procedure TContainer.RaiseKeyAlreadyInMap;
Begin
  Raise
  EContainerError.Create(S_KeyAlreadyInMap);
End;

{--- TContainer.RaiseKeyNotInMap ---}
Procedure TContainer.RaiseKeyNotInMap;
Begin
  Raise
  EContainerError.Create(S_KeyNotInMap);
End;

{--- TContainer.RaiseMethodNotRedefined ---}
Procedure TContainer.RaiseMethodNotRedefined;
Begin
  Raise
  EContainerError.Create(S_MethodNotRedefined);
End;

{--- TContainer.Unused ---}
{$PUSH}
{$HINTS OFF}
Procedure TContainer.Unused(P: Pointer); Inline;
Begin
End;
{$POP}

{=======================}
{=== TAbstractVector ===}
{=======================}

{--- TAbstractVector.CheckIndex ---}
Procedure TAbstractVector.CheckIndex(Index: Integer);
Begin
  If (Index < 0) Or (Index >= fSize) Then
    RaiseIndexOutOfRange;
End;

{--- TAbstractVector.CheckIndexForAdd ---}
Procedure TAbstractVector.CheckIndexForAdd(Index: Integer);
Begin
  If (Index < 0) Or (Index > fSize) Then
    RaiseIndexOutOfRange;
End;

{--- TAbstractVector.Clear ---}
Procedure TAbstractVector.Clear;
Begin
  Resize(0);
End;

{--- TAbstractVector.Delete ---}
Procedure TAbstractVector.Delete(Position: Integer; Count: Integer);
Var

  CountAtEnd: Integer;
Begin
  CheckIndex(
    Position);

  If Position + Count > fSize Then
    Count :=
      fSize - Position;

  If Count > 0 Then
  Begin
    CountAtEnd :=
      fSize - (Position + Count);
    If CountAtEnd > 0 Then
      Move(
        Position + Count, Position, CountAtEnd);

    fSize :=
      fSize - Count;
  End;
End;

{--- TAbstractVector.DeleteFirst ---}
Procedure TAbstractVector.DeleteFirst(Count: Integer);
Begin
  If Count > 0 Then
    Delete(
      0, Count);
End;

{--- TAbstractVector.DeleteLast ---}
Procedure TAbstractVector.DeleteLast(Count: Integer);
Begin
  If Count > 0 Then
    Resize(
      fSize - Count);
End;

{--- TAbstractVector.DeleteRange ---}
Procedure TAbstractVector.DeleteRange(PosFrom, PosTo: Integer);
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo >= PosFrom Then
    Delete(
      PosFrom, PosTo - PosFrom + 1);
End;

{--- TAbstractVector.InsertSpace ---}
Procedure TAbstractVector.InsertSpace(Position: Integer; Count: Integer);
Begin
  CheckIndexForAdd(
    Position);
  InsertSpaceFast(
    Position, Count);
End;

{--- TAbstractVector.IsEmpty ---}
Function TAbstractVector.IsEmpty: Boolean;
Begin
  Result :=
    (fSize = 0);
End;

{--- TAbstractVector.Reserve ---}
Procedure TAbstractVector.Reserve(MinCapacity: Integer);
Var

  NewCapacity: Integer;
Begin
  If MinCapacity > Capacity Then
  Begin
    NewCapacity :=
      (Capacity * 3) Div 2;
    If NewCapacity < MinCapacity Then
      NewCapacity :=
        MinCapacity;
    SetCapacity(
      NewCapacity);
  End;
End;

{--- TAbstractVector.Resize ---}
Procedure TAbstractVector.Resize(NewSize: Integer);
Begin
  If NewSize > fSize Then
    Reserve(
      NewSize);

  If NewSize < 0 Then
    NewSize := 0;

  fSize := NewSize;
End;

{--- TAbstractVector.Reverse ---}
Procedure TAbstractVector.Reverse;
Begin
  If fSize > 1 Then
    ReverseRange(
      0, fSize - 1);
End;

{--- TAbstractVector.ReverseRange ---}
Procedure TAbstractVector.ReverseRange(PosFrom, PosTo: Integer);
Var

  TmpIndex: Integer;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo < PosFrom Then
  Begin
    TmpIndex :=
      PosFrom;
    PosFrom :=
      PosTo;
    PosTo :=
      TmpIndex;
  End;

  While
    PosFrom < PosTo Do
  Begin
    SwapFast(
      PosFrom, PosTo);
    Inc(
      PosFrom);
    Dec(
      PosTo);
  End;
End;

{--- TAbstractVector.Shuffle ---}
Procedure TAbstractVector.Shuffle;
Begin
  If fSize > 1 Then
    Shuffle(
      0, fSize - 1);
End;

{--- TAbstractVector.Shuffle ---}
Procedure TAbstractVector.Shuffle(PosFrom, PosTo: Integer);
Var

  I, J: Integer;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  I := PosTo;
  While
    I > PosFrom Do
  Begin
    J :=
      Random(I - PosFrom) + PosFrom;
    If J <> I Then
      SwapFast(J, I);
    Dec(I);
  End;
End;

{--- TAbstractVector.Swap ---}
Procedure TAbstractVector.Swap(I, J: Integer);
Begin
  CheckIndex(I);
  CheckIndex(J);
  SwapFast(I, J);
End;

{--- TAbstractVector.ToString ---}
Function TAbstractVector.ToString: String;
Var

  I: Integer;
Begin
  Result := '[';

  If fSize > 0 Then
  Begin
    For
      I := 0 To fSize - 2 Do
      Result :=
        Result + ItemToString(I) + ', ';
    Result :=
      Result + ItemToString(fSize - 1);
  End;

  Result :=
    Result + ']';
End;

{==================}
{=== TGenVector ===}
{==================}

{--- TGenVector.Append ---}
Procedure TGenVector.Append(Const Item: _TItem_);
Begin
  Insert(
    fSize, Item);
End;

{--- TGenVector.AppendAll ---}
Procedure TGenVector.AppendAll(Src: TGenVector);
Begin
  InsertAll(
    fSize, Src);
End;

{--- TGenVector.AppendRange ---}
Procedure TGenVector.AppendRange(Src: TGenVector; PosFrom, PosTo: Integer);
Begin
  InsertRange(
    fSize, Src, PosFrom, PosTo);
End;

{--- TGenVector.BinarySearch ---}
Function TGenVector.BinarySearch(Const Item: _TItem_): Integer;
Begin
  Result :=
    BinarySearch(Item, fOnCompareItems);
End;

{--- TGenVector.BinarySearch ---}
Function TGenVector.BinarySearch(Const Item: _TItem_;
  Comparator: TCompareItems): Integer;
Begin
  If fSize > 0 Then
    Result :=
      BinarySearch(Item, 0, fSize - 1, Comparator)
  Else
    Result := -1;
End;

{--- TGenVector.BinarySearch ---}
Function TGenVector.BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    BinarySearch(Item, PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenVector.BinarySearch ---}
Function TGenVector.BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer;
  Comparator: TCompareItems): Integer;
Var

  Low, Mid, High, Cmp: Integer;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  Low := PosFrom;
  Mid := -1;
  High := PosTo;

  While
    Low <= High Do
  Begin
    Mid :=
      (Low + High) Div 2;
    Cmp :=
      Comparator(fItems[Mid], Item);

    If Cmp = 0 Then
    Begin
      Result := Mid;
      Exit;
    End;

    If Cmp < 0 Then
      Low :=
        Mid + 1
    Else
      High :=
        Mid - 1;
  End;

  If Mid < 0 Then
    Result := -1
  Else
    If Comparator(fItems[Mid], Item) > 0 Then
      Result := -Mid - 1
    Else
      Result := -Mid - 2;
End;

{--- TGenVector.DefaultCompareItems ---}
Function TGenVector.DefaultCompareItems(Const A, B: _TItem_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenVector.Contains ---}
Function TGenVector.Contains(Const Item: _TItem_): Boolean;
Begin
  Result :=
    Contains(Item, fOnCompareItems);
End;

{--- TGenVector.Contains ---}
Function TGenVector.Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;
Begin
  If fSize = 0 Then
    Result := False
  Else
    Result :=
      (FindIndex(Item, 0, Comparator) >= 0);
End;

{--- TGenVector.Create ---}
Constructor TGenVector.Create(InitialCapacity: Integer);
Begin
  If InitialCapacity < 0 Then
    InitialCapacity := 16;

  fSize := 0;

  SetCapacity(
    InitialCapacity);

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
End;

{--- TGenVector.Destroy ---}
Destructor TGenVector.Destroy;
Begin
  SetCapacity(0);
  Inherited
  Destroy;
End;

{--- TGenVector.Equals ---}
Function TGenVector.Equals(Obj: TObject): Boolean;
Begin
  Result :=
    Equals(Obj, fOnCompareItems);
End;

{--- TGenVector.Equals ---}
Function TGenVector.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
Var

  Vector: TGenVector;
  I: Integer;
Begin
  If Obj = Self Then
    Result := True
  Else
    If Obj Is TGenVector Then
    Begin
      Vector :=
        Obj As TGenVector;

      If fSize <> Vector.fSize Then
        Result := False
      Else
      Begin
        Result := True;
        For
          I := 0 To fSize - 1 Do
          If Comparator(fItems[I], Vector.fItems[I]) <> 0 Then
          Begin
            Result :=
              False;
            Break;
          End;
      End;
    End
    Else
      Result :=
        False;
End;

{--- TGenVector.EnumeratorGet ---}
Function TGenVector.EnumeratorGet(Const Pos: Integer): _TItem_;
Begin
  Result :=
    fItems[Pos];
End;

{--- TGenVector.EnumeratorNext ---}
Function TGenVector.EnumeratorNext(Var Pos: Integer): Boolean;
Begin
  Inc(Pos);
  Result :=
    Pos < fSize;
End;

{--- TGenVector.Fill ---}
Procedure TGenVector.Fill(Index, Count: Integer; Const Value: _TItem_);
Var

  I: Integer;
Begin
  If Count > 0 Then
    For
      I := Index To Index + (Count - 1) Do
      fItems
        [I] := Value;
End;

{--- TGenVector.FindIndex ---}
Function TGenVector.FindIndex(Const Item: _TItem_): Integer;
Begin
  Result :=
    FindIndex(Item, fOnCompareItems);
End;

{--- TGenVector.FindIndex ---}
Function TGenVector.FindIndex(Const Item: _TItem_; Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    Result := -1
  Else
    Result :=
      FindIndex(Item, 0, Comparator);
End;

{--- TGenVector.FindIndex ---}
Function TGenVector.FindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
Begin
  Result :=
    FindIndex(Item, PosFrom, fOnCompareItems);
End;

{--- TGenVector.FindIndex ---}
Function TGenVector.FindIndex(Const Item: _TItem_; PosFrom: Integer;
  Comparator: TCompareItems): Integer;
Var

  I: Integer;
Begin
  CheckIndex(
    PosFrom);

  Result := -1;

  For
    I := PosFrom To fSize - 1 Do
    If Comparator(fItems[I], Item) = 0 Then
    Begin
      Result := I;
      Break;
    End;
End;

{--- TGenVector.FirstItem ---}
Function TGenVector.FirstItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fItems[0];
End;

{--- TGenVector.GetEnumerator ---}
Function TGenVector.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(-1, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenVector.GetItem ---}
Function TGenVector.GetItem(Position: Integer): _TItem_;
Begin
  CheckIndex(
    Position);
  Result :=
    fItems[Position];
End;

{--- TGenVector.GetItemFast ---}
Function TGenVector.GetItemFast(Position: Integer): _TItem_;
Begin
  Result :=
    fItems[Position];
End;

{--- TGenVector.GetItemPtr ---}
Function TGenVector.GetItemPtr(Position: Integer): PItem;
Begin
  CheckIndex(
    Position);
  Result := @fItems[Position];
End;

{--- TGenVector.GetItemPtrFast ---}
Function TGenVector.GetItemPtrFast(Position: Integer): PItem;
Begin
  Result := @fItems[Position];
End;

{--- TGenVector.Insert ---}
Procedure TGenVector.Insert(Before: Integer; Const Item: _TItem_; Count: Integer);
Begin
  CheckIndexForAdd(
    Before);

  If Count > 0 Then
  Begin
    InsertSpaceFast(
      Before, Count);
    Fill(
      Before, Count, Item);
  End;
End;

{--- TGenVector.InsertAll ---}
Procedure TGenVector.InsertAll(Before: Integer; Src: TGenVector);
Begin
  If Src.fSize > 0 Then
    InsertRange(
      Before, Src, 0, Src.fSize - 1);
End;

{--- TGenVector.InsertionSort ---}
Procedure TGenVector.InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
Var

  I, J: Integer;
  Tmp, Item: _TItem_;
Begin
  If PosFrom >= PosTo Then
    Exit;

  For
    I := PosFrom + 1 To PosTo Do
  Begin
    Tmp :=
      fItems[I];

    J := I - 1;
    While
      (J >= PosFrom) Do
    Begin
      Item :=
        fItems[J];
      If Comparator(Item, Tmp) <= 0 Then
        Break;
      fItems
        [J + 1] := fItems[J];
      Dec(J);
    End;

    fItems
      [J + 1] := Tmp;
  End;
End;

{--- TGenVector.Quicksort ---}
Procedure TGenVector.Quicksort(Left, Right: Integer; Comparator: TCompareItems);
Var

  I, J: Integer;
  Pivot: _TItem_;
Begin
  If Right - Left <= 15 Then
  Begin
    InsertionSort(
      Left, Right, Comparator);
    Exit;
  End;

  I := Left;
  J := Right;
  Pivot :=
    fItems[(Left + Right) Div 2];
  Repeat
    While
      Comparator(Pivot, fItems[I]) > 0 Do
      Inc(I);

    While
      Comparator(Pivot, fItems[J]) < 0 Do
      Dec(J);

    If I <= J Then
    Begin
      SwapFast(I, J);
      Dec(J);
      Inc(I);
    End;
  Until
    I > J;

  If Left < J Then
    QuickSort(
      Left, J, Comparator);

  If I < Right Then
    QuickSort(
      I, Right, Comparator);
End;

{--- TGenVector.InsertRange ---}
Procedure TGenVector.InsertRange(Before: Integer; Src: TGenVector;
  PosFrom, PosTo: Integer);
Var

  Count: Integer;
Begin
  CheckIndexForAdd(
    Before);
  Src.
    CheckIndex(PosFrom);
  Src.
    CheckIndex(PosTo);

  Count :=
    PosTo - PosFrom + 1;
  If Count > 0 Then
  Begin
    InsertSpaceFast(
      Before, Count);
    RealMove(
      Src, Self, PosFrom, Before, Count);
  End;
End;

{--- TGenVector.InsertSpaceFast ---}
Procedure TGenVector.InsertSpaceFast(Position, Count: Integer);
Var

  ItemsAfterPos: Integer;
Begin
  If Count > 0 Then
  Begin
    ItemsAfterPos :=
      fSize - Position;
    Resize(
      fSize + Count);
    If ItemsAfterPos > 0 Then
      Move(
        Position, Position + Count, ItemsAfterPos);
  End;
End;

{--- TGenVector.ItemToString ---}
Function TGenVector.ItemToString(Index: Integer): String;
Begin
  Result :=
    fOnItemToString(fItems[Index]);
End;

{--- TGenVector.IsSorted ---}
Function TGenVector.IsSorted: Boolean;
Begin
  Result :=
    IsSorted(fOnCompareItems);
End;

{--- TGenVector.IsSorted ---}
Function TGenVector.IsSorted(Comparator: TCompareItems): Boolean;
Var

  I: Integer;
Begin
  Result := True;

  If fSize > 1 Then
    For
      I := 1 To fSize - 1 Do
      If Comparator(fItems[I], fItems[I - 1]) < 0 Then
      Begin
        Result :=
          False;
        Break;
      End;
End;

{--- TGenVector.DefaultItemToString ---}
Function TGenVector.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenVector.Iterate ---}
Procedure TGenVector.Iterate(Process: TProcessItem);
Begin
  Iterate(
    Process, 0, fSize - 1);
End;

{--- TGenVector.Iterate ---}
Procedure TGenVector.Iterate(Process: TProcessItem; Const PosFrom, PosTo: Integer);
Var

  I: Integer;
  P: PItem;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  P := @fItems[PosFrom];
  For
    I := PosFrom To PosTo Do
  Begin
    Process(P^);
    P := P + 1;
  End;
End;

{--- TGenVector.LastItem ---}
Function TGenVector.LastItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fItems[fSize - 1];
End;

{--- TGenVector.MaxPos ---}
Function TGenVector.MaxPos(PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    MaxPos(PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenVector.MaxPos ---}
Function TGenVector.MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
Var

  I: Integer;
  Max: _TItem_;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo < PosFrom Then
  Begin
    I := PosFrom;
    PosFrom :=
      PosTo;
    PosTo := I;
  End;

  Max :=
    fItems[PosFrom];
  Result := PosFrom;
  For
    I := PosFrom + 1 To PosTo Do
    If Comparator(fItems[I], Max) > 0 Then
    Begin
      Result := I;
      Max :=
        fItems[I];
    End;
End;

{--- TGenVector.MaxPos ---}
Function TGenVector.MaxPos: Integer;
Begin
  Result :=
    MaxPos(fOnCompareItems);
End;

{--- TGenVector.MaxPos ---}
Function TGenVector.MaxPos(Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    MaxPos(0, fSize - 1, Comparator);
End;

{--- TGenVector.Merge ---}
Procedure TGenVector.Merge(Src: TGenVector);
Begin
  Merge(
    Src, fOnCompareItems);
End;

{--- TGenVector.Merge ---}
Procedure TGenVector.Merge(Src: TGenVector; Comparator: TCompareItems);
Var

  A, B, C: Integer;
Begin
  If Src.fSize = 0 Then
    Exit;

  If fSize = 0 Then
    AppendAll(Src)
  Else
    If Comparator(Src.FirstItem, LastItem) >= 0 Then
      AppendAll(Src)
    Else
      If Comparator(FirstItem, Src.LastItem) >= 0 Then
        PrependAll(Src)
      Else
      Begin
        A := fSize - 1;
        B :=
          Src.fSize - 1;

        InsertSpace(
          fSize, Src.fSize);
        C := fSize - 1;

        While
          C > 0 Do
        Begin
          If Comparator(fItems[A], Src.fItems[B]) > 0 Then
          Begin
            fItems
              [C] := fItems[A];
            Dec(A);
            If A < 0 Then
              Break;
          End
          Else
          Begin
            fItems
              [C] := Src.fItems[B];
            Dec(B);
            If B < 0 Then
              Break;
          End;
          Dec(C);
        End;

        If (C >= 0) And (B >= 0) Then
          While
            B >= 0 Do
          Begin
            fItems
              [B] := Src.fItems[B];
            Dec(B);
          End;

      End;
  Src.Clear;
End;

{--- TGenVector.MinPos ---}
Function TGenVector.MinPos(PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    MinPos(PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenVector.MinPos ---}
Function TGenVector.MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
Var

  I: Integer;
  Min: _TItem_;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo < PosFrom Then
  Begin
    I := PosFrom;
    PosFrom :=
      PosTo;
    PosTo := I;
  End;

  Result := -1;
  Min :=
    fItems[PosFrom];
  Result := PosFrom;
  For
    I := PosFrom + 1 To PosTo Do
    If Comparator(fItems[I], Min) < 0 Then
    Begin
      Result := I;
      Min :=
        fItems[I];
    End;
End;

{--- TGenVector.MinPos ---}
Function TGenVector.MinPos: Integer;
Begin
  Result :=
    MinPos(fOnCompareItems);
End;

{--- TGenVector.MinPos ---}
Function TGenVector.MinPos(Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    MinPos(0, fSize - 1, Comparator);
End;

{--- TGenVector.Move ---}
Procedure TGenVector.Move(Src, Dst, Count: Integer);
Begin
  CheckIndex(Src);
  CheckIndex(Dst);

  If Count > 0 Then
  Begin
    If Src + Count > fSize Then
      Count :=
        fSize - Src;

    If Dst + Count > fSize Then
      Count :=
        fSize - Dst;

    If Count > 0 Then
      RealMove(
        Self, Self, Src, Dst, Count);
  End;
End;

{--- TGenVector.Prepend ---}
Procedure TGenVector.Prepend(Const Item: _TItem_; Count: Integer);
Begin
  Insert(
    0, Item, Count);
End;

{--- TGenVector.PrependAll ---}
Procedure TGenVector.PrependAll(Src: TGenVector);
Begin
  InsertAll(
    0, Src);
End;

{--- TGenVector.PrependRange ---}
Procedure TGenVector.PrependRange(Src: TGenVector; PosFrom, PosTo: Integer);
Begin
  InsertRange(
    0, Src, PosFrom, PosTo);
End;

{--- TGenVector.ReadFirstItem ---}
Procedure TGenVector.ReadFirstItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value := fItems[0];
End;

{--- TGenVector.ReadItem ---}
Procedure TGenVector.ReadItem(Position: Integer; out Value: _TItem_);
Begin
  CheckIndex(
    Position);
  Value :=
    fItems[Position];
End;

{--- TGenVector.ReadItemFast ---}
Procedure TGenVector.ReadItemFast(Position: Integer; out Value: _TItem_);
Begin
  Value :=
    fItems[Position];
End;

{--- TGenVector.ReadLastItem ---}
Procedure TGenVector.ReadLastItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fItems[fSize - 1];
End;

{--- TGenVector.Sort ---}
Procedure TGenVector.Sort(PosFrom, PosTo: Integer);
Begin
  Sort(
    PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenVector.Sort ---}
Procedure TGenVector.Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosFrom >= PosTo Then
    Exit;

  Quicksort(
    PosFrom, PosTo, Comparator);
End;

{--- TGenVector.Sort ---}
Procedure TGenVector.Sort;
Begin
  Sort(
    fOnCompareItems);
End;

{--- TGenVector.Sort ---}
Procedure TGenVector.Sort(Comparator: TCompareItems);
Begin
  If fSize > 1 Then
    Sort(
      0, fSize - 1, Comparator);
End;

{--- TGenVector.RealMove ---}
Class
Procedure TGenVector.RealMove(Src, Dst: TGenVector; SrcFirst, DstFirst, Count: Integer);
Var

  SrcLast, I, DstCurrent: Integer;
Begin
  SrcLast :=
    SrcFirst + Count - 1;
  If (Src = Dst) And ((DstFirst >= SrcFirst) And (DstFirst <= SrcLast)) Then
  Begin
    DstCurrent :=
      DstFirst + Count - 1;
    For
      I := SrcLast Downto SrcFirst Do
    Begin
      Dst.
        fItems[DstCurrent] := Src.fItems[I];
      Dec(
        DstCurrent);
    End;
  End
  Else
  Begin
    DstCurrent :=
      DstFirst;
    For
      I := SrcFirst To SrcLast Do
    Begin
      Dst.
        fItems[DstCurrent] := Src.fItems[I];
      Inc(
        DstCurrent);
    End;
  End;
End;

{--- TGenVector.Replace ---}
Procedure TGenVector.Replace(Index, Count: Integer; Const Value: _TItem_);
Begin
  CheckIndex(
    Index);

  If Count > 0 Then
  Begin
    If Index + Count >= fSize Then
      Count :=
        fSize - Index;

    If Count > 0 Then
      Fill(
        Index, Count, Value);
  End;
End;

{--- TGenVector.ReverseFindIndex ---}
Function TGenVector.ReverseFindIndex(Const Item: _TItem_): Integer;
Begin
  Result :=
    ReverseFindIndex(Item, fOnCompareItems);
End;

{--- TGenVector.ReverseFindIndex ---}
Function TGenVector.ReverseFindIndex(Const Item: _TItem_;
  Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    Result := -1
  Else
    Result :=
      ReverseFindIndex(Item, fSize - 1, Comparator);
End;

{--- TGenVector.ReverseFindIndex ---}
Function TGenVector.ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
Begin
  Result :=
    ReverseFindIndex(Item, PosFrom, fOnCompareItems);
End;

{--- TGenVector.ReverseFindIndex ---}
Function TGenVector.ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer;
  Comparator: TCompareItems): Integer;
Var

  I: Integer;
Begin
  CheckIndex(
    PosFrom);

  Result := -1;
  For
    I := PosFrom Downto 0 Do
    If Comparator(fItems[I], Item) = 0 Then
    Begin
      Result := I;
      Break;
    End;
End;

{--- TGenVector.SetCapacity ---}
Procedure TGenVector.SetCapacity(ACapacity: Integer);
Begin
  SetLength(
    fItems, ACapacity);
  fCapacity :=
    ACapacity;
End;

{--- TGenVector.SetOnCompareItems ---}
Procedure TGenVector.SetOnCompareItems(AValue: TCompareItems);
Begin
  If AValue = nil Then
    fOnCompareItems := @DefaultCompareItems
  Else
    fOnCompareItems :=
      AValue;
End;

{--- TGenVector.SetOnItemToString ---}
Procedure TGenVector.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fOnItemToString := @DefaultItemToString
  Else
    fOnItemToString :=
      AValue;
End;

{--- TGenVector.SetItem ---}
Procedure TGenVector.SetItem(Position: Integer; Const Value: _TItem_);
Begin
  CheckIndex(
    Position);
  fItems
    [Position] := Value;
End;

{--- TGenVector.SetItemFast ---}
Procedure TGenVector.SetItemFast(Position: Integer; Const Value: _TItem_);
Begin
  fItems
    [Position] := Value;
End;

{--- TGenVector.SwapFast ---}
Procedure TGenVector.SwapFast(I, J: Integer);
Var

  Temp: _TItem_;
Begin
  Temp :=
    fItems[I];
  fItems
    [I] := fItems[J];
  fItems
    [J] := Temp;
End;

{=================}
{=== TGenDeque ===}
{=================}

{--- TGenDeque.Append ---}
Procedure TGenDeque.Append(Const Item: _TItem_; Count: Integer);
Begin
  Insert(
    fSize, Item, Count);
End;

{--- TGenDeque.AppendAll ---}
Procedure TGenDeque.AppendAll(Src: TGenDeque);
Begin
  InsertAll(
    fSize, Src);
End;

{--- TGenDeque.AppendRange ---}
Procedure TGenDeque.AppendRange(Src: TGenDeque; PosFrom, PosTo: Integer);
Begin
  InsertRange(
    fSize, Src, PosFrom, PosTo);
End;

{--- TGenDeque.BinarySearch ---}
Function TGenDeque.BinarySearch(Const Item: _TItem_): Integer;
Begin
  Result :=
    BinarySearch(Item, fOnCompareItems);
End;

{--- TGenDeque.BinarySearch ---}
Function TGenDeque.BinarySearch(Const Item: _TItem_; Comparator: TCompareItems): Integer;
Begin
  If fSize > 0 Then
    Result :=
      BinarySearch(Item, 0, fSize - 1, Comparator)
  Else
    Result := -1;
End;

{--- TGenDeque.BinarySearch ---}
Function TGenDeque.BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    BinarySearch(Item, PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenDeque.BinarySearch ---}
Function TGenDeque.BinarySearch(Const Item: _TItem_; PosFrom, PosTo: Integer;
  Comparator: TCompareItems): Integer;
Var

  Low, Mid, High, Cmp: Integer;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  Low := PosFrom;
  Mid := -1;
  High := PosTo;

  While
    Low <= High Do
  Begin
    Mid :=
      (Low + High) Div 2;
    Cmp :=
      Comparator(fItems[IndexToRank(Mid)], Item);

    If Cmp = 0 Then
    Begin
      Result := Mid;
      Exit;
    End;

    If Cmp < 0 Then
      Low :=
        Mid + 1
    Else
      High :=
        Mid - 1;
  End;

  If Mid < 0 Then
    Result := -1
  Else
    If Comparator(fItems[IndexToRank(Mid)], Item) > 0 Then
      Result := -Mid - 1
    Else
      Result := -Mid - 2;
End;

{--- TGenDeque.DefaultCompareItems ---}
Function TGenDeque.DefaultCompareItems(Const A, B: _TItem_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenDeque.Contains ---}
Function TGenDeque.Contains(Const Item: _TItem_): Boolean;
Begin
  Result :=
    Contains(Item, fOnCompareItems);
End;

{--- TGenDeque.Contains ---}
Function TGenDeque.Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;
Begin
  Result :=
    (FindIndex(Item, Comparator) >= 0);
End;

{--- TGenDeque.Create ---}
Constructor TGenDeque.Create(InitialCapacity: Integer);
Begin
  fSize := 0;

  If InitialCapacity < 0 Then
    InitialCapacity := 16;

  fCapacity :=
    InitialCapacity;
  SetLength(
    fItems, fCapacity);

  fStart := 0;

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
End;

{--- TGenDeque.Destroy ---}
Destructor TGenDeque.Destroy;
Begin
  SetLength(
    fItems, 0);
  Inherited
  Destroy;
End;

{--- TGenDeque.DecRank ---}
Procedure TGenDeque.DecRank(Var Rank: Integer);
Begin
  If Rank = 0 Then
    Rank :=
      fCapacity - 1
  Else
    Dec(
      Rank);
End;

{--- TGenDeque.Equals ---}
Function TGenDeque.Equals(Deque: TGenDeque; Comparator: TCompareItems): Boolean;
Var

  I, IRank, JRank: Integer;
Begin
  If fSize <> Deque.fSize Then
    Result := False
  Else
  Begin
    Result := True;
    IRank :=
      fStart;
    JRank :=
      Deque.fStart;
    For
      I := 0 To fSize - 1 Do
    Begin
      If Comparator(fItems[IRank], Deque.fItems[JRank]) <> 0 Then
      Begin
        Result :=
          False;
        Break;
      End;
      IncRank(
        IRank);
      Deque.
        IncRank(JRank);
    End;
  End;
End;

{--- TGenDeque.EnumeratorGet ---}
Function TGenDeque.EnumeratorGet(Const Pos: Integer): _TItem_;
Begin
  Result :=
    fItems[IndexToRank(Pos)];
End;

{--- TGenDeque.EnumeratorNext ---}
Function TGenDeque.EnumeratorNext(Var Pos: Integer): Boolean;
Begin
  Inc(Pos);
  Result :=
    Pos < fSize;
End;

{--- TGenDeque.Equals ---}
Function TGenDeque.Equals(Obj: TObject): Boolean;
Begin
  Result :=
    Equals(Obj, fOnCompareItems);
End;

{--- TGenDeque.Equals ---}
Function TGenDeque.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
Begin
  If Obj = Self Then
    Result := True
  Else
    If Obj Is TGenDeque Then
      Result :=
        Equals(Obj As TGenDeque, Comparator)
    Else
      Result :=
        False;
End;

{--- TGenDeque.FindIndex ---}
Function TGenDeque.FindIndex(Const Item: _TItem_): Integer;
Begin
  Result :=
    FindIndex(Item, fOnCompareItems);
End;

{--- TGenDeque.FindIndex ---}
Function TGenDeque.FindIndex(Const Item: _TItem_; Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    Result := -1
  Else
    Result :=
      FindIndex(Item, 0, Comparator);
End;

{--- TGenDeque.Fill ---}
Procedure TGenDeque.Fill(Index, Count: Integer; Const Value: _TItem_);
Begin
  Index :=
    IndexToRank(Index);
  While
    Count > 0 Do
  Begin
    fItems
      [Index] := Value;
    IncRank(
      Index);
    Dec(
      Count);
  End;
End;

{--- TGenDeque.FindIndex ---}
Function TGenDeque.FindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
Begin
  Result :=
    FindIndex(Item, PosFrom, fOnCompareItems);
End;

{--- TGenDeque.FindIndex ---}
Function TGenDeque.FindIndex(Const Item: _TItem_; PosFrom: Integer;
  Comparator: TCompareItems): Integer;
Var

  I, Pos: Integer;
Begin
  CheckIndex(
    PosFrom);

  Result := -1;
  Pos :=
    IndexToRank(PosFrom);
  For
    I := PosFrom To fSize - 1 Do
  Begin
    If Comparator(fItems[Pos], Item) = 0 Then
    Begin
      Result := I;
      Break;
    End;
    IncRank(Pos);
  End;
End;

{--- TGenDeque.FirstItem ---}
Function TGenDeque.FirstItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fItems[fStart];
End;

{--- TGenDeque.GetEnumerator ---}
Function TGenDeque.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(-1, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenDeque.GetItem ---}
Function TGenDeque.GetItem(Position: Integer): _TItem_;
Begin
  CheckIndex(
    Position);
  Result :=
    fItems[IndexToRank(Position)];
End;

{--- TGenDeque.GetItemPtr ---}
Function TGenDeque.GetItemPtr(Position: Integer): PItem;
Begin
  CheckIndex(
    Position);
  Result := @fItems[IndexToRank(Position)];
End;

{--- TGenDeque.GetItemFast ---}
Function TGenDeque.GetItemFast(Position: Integer): _TItem_;
Begin
  Result :=
    fItems[IndexToRank(Position)];
End;

{--- TGenDeque.GetItemPtrFast ---}
Function TGenDeque.GetItemPtrFast(Position: Integer): PItem;
Begin
  Result := @fItems[IndexToRank(Position)];
End;

{--- TGenDeque.IncRank ---}
Procedure TGenDeque.IncRank(Var Rank: Integer);
Begin
  If Rank = fCapacity - 1 Then
    Rank := 0
  Else
    Inc(
      Rank);
End;

{--- TGenDeque.IncreaseCapacity ---}
Procedure TGenDeque.IncreaseCapacity(ACapacity: Integer);
Var

  Dst: Integer;
  ItemsAtBegining, ItemsAtEnd: Integer;
Begin
  SetLength(
    fItems, ACapacity);

  If fStart + fSize >= fCapacity Then { Are items in 2 parts ? }
  Begin
    ItemsAtEnd :=
      fCapacity - fStart;
    ItemsAtBegining :=
      fSize - ItemsAtEnd;

    If ItemsAtEnd < ItemsAtBegining Then
    Begin
      Dst :=
        ACapacity - ItemsAtEnd;
      RealMoveRank(
        fStart, Dst, ItemsAtEnd);
      fStart := Dst;
    End
    Else
    Begin
      Dst :=
        fStart + ItemsAtEnd;
      RealMoveRank(
        0, Dst, ItemsAtBegining);
    End;
  End;

  fCapacity :=
    ACapacity;
End;

{--- TGenDeque.IndexToRank ---}
Function TGenDeque.IndexToRank(Index: Integer): Integer;
Var

  AtEnd: Integer;
Begin
  AtEnd :=
    fCapacity - fStart;
  If Index < AtEnd Then
    Result :=
      fStart + Index
  Else
    Result :=
      Index - AtEnd;
End;

{--- TGenDeque.Insert ---}
Procedure TGenDeque.Insert(Before: Integer; Const Item: _TItem_; Count: Integer);
Begin
  CheckIndexForAdd(
    Before);

  If Count <= 0 Then
    Exit;

  InsertSpaceFast(
    Before, Count);
  Fill(
    Before, Count, Item);
End;

{--- TGenDeque.InsertAll ---}
Procedure TGenDeque.InsertAll(Before: Integer; Src: TGenDeque);
Begin
  If Src.fSize > 0 Then
    InsertRange(
      Before, Src, 0, Src.fSize - 1);
End;

{--- TGenDeque.InsertionSort ---}
Procedure TGenDeque.InsertionSort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
Var

  I, J: Integer;
  IRank, JRank, NextJRank: Integer;
  Tmp, Item: _TItem_;
Begin
  If PosFrom >= PosTo Then
    Exit;

  IRank :=
    IndexToRank(PosFrom + 1);
  For
    I := PosFrom + 1 To PosTo Do
  Begin
    Tmp :=
      fItems[IRank];

    J :=
      I - 1;
    JRank :=
      IRank;
    DecRank(
      JRank);
    While
      (J >= PosFrom) Do
    Begin
      Item :=
        fItems[JRank];
      If Comparator(Item, Tmp) <= 0 Then
        Break;
      NextJRank :=
        JRank;
      IncRank(
        NextJRank);
      fItems
        [NextJRank] := fItems[JRank];
      Dec(J);
      DecRank(
        JRank);
    End;

    fItems
      [IndexToRank(J + 1)] := Tmp;
    IncRank(
      IRank);
  End;
End;

{--- TGenDeque.Quicksort ---}
Procedure TGenDeque.Quicksort(Left, Right: Integer; Comparator: TCompareItems);
Var

  I, J: Integer;
  Pivot: _TItem_;
Begin
  If Right - Left <= 15 Then
  Begin
    InsertionSort(
      Left, Right, Comparator);
    Exit;
  End;

  I := Left;
  J := Right;
  Pivot :=
    fItems[IndexToRank((Left + Right) Div 2)];
  Repeat
    While
      Comparator(Pivot, fItems[IndexToRank(I)]) > 0 Do
      Inc(I);

    While
      Comparator(Pivot, fItems[IndexToRank(J)]) < 0 Do
      Dec(J);

    If I <= J Then
    Begin
      SwapFast(I, J);
      Dec(J);
      Inc(I);
    End;
  Until
    I > J;

  If Left < J Then
    QuickSort(
      Left, J, Comparator);

  If I < Right Then
    QuickSort(
      I, Right, Comparator);
End;

{--- TGenDeque.InsertRange ---}
Procedure TGenDeque.InsertRange(Before: Integer; Src: TGenDeque;
  PosFrom, PosTo: Integer);
Var

  Count: Integer;
Begin
  CheckIndexForAdd(
    Before);
  Src.
    CheckIndex(PosFrom);
  Src.
    CheckIndex(PosTo);

  Count :=
    PosTo - PosFrom + 1;
  If Count > 0 Then
  Begin
    InsertSpaceFast(
      Before, Count);
    RealMoveIndex(
      Src, Self, PosFrom, Before, Count);
  End;
End;

{--- TGenDeque.InsertSpaceFast ---}
Procedure TGenDeque.InsertSpaceFast(Position, Count: Integer);
Var

  Rank: Integer;
  NewStart: Integer;
  ItemsToMove: Integer;
Begin
  If Count <= 0 Then
    Exit;

  If Position = 0 Then
  Begin
    Resize(
      fSize + Count);

    NewStart :=
      fStart - Count;
    If NewStart < 0 Then
      fStart :=
        fCapacity + NewStart
    Else
      fStart :=
        NewStart;
  End
  Else
    If Position = fSize Then
    Begin
      Resize(
        fSize + Count);
    End
    Else
    Begin
      Resize(
        fSize + Count);
      Rank :=
        IndexToRank(Position);

      If (Rank >= fStart) And (fStart + fSize > fCapacity) Then
      Begin
        ItemsToMove :=
          Rank - fStart;
        If ItemsToMove > 0 Then
          RealMoveRank(
            fStart, fStart - Count, ItemsToMove);
        fStart :=
          fStart - Count;
      End
      Else
      Begin
        ItemsToMove :=
          fSize - Position - Count;

        If ItemsToMove > 0 Then
          RealMoveRank(
            Rank, Rank + Count, ItemsToMove);
      End;
    End;
End;

{--- TGenDeque.ItemToString ---}
Function TGenDeque.ItemToString(Index: Integer): String;
Begin
  Result :=
    fOnItemToString(fItems[IndexToRank(Index)]);
End;

{--- TGenDeque.RankToIndex ---}
Function TGenDeque.RankToIndex(Rank: Integer): Integer;
Begin
  If Rank >= fStart Then
    Result :=
      Rank - fStart
  Else
    Result :=
      Rank + (fCapacity - fStart);
End;

{--- TGenDeque.IsSorted ---}
Function TGenDeque.IsSorted: Boolean;
Begin
  Result :=
    IsSorted(fOnCompareItems);
End;

{--- TGenDeque.IsSorted ---}
Function TGenDeque.IsSorted(Comparator: TCompareItems): Boolean;
Var

  I, Rank, PrevRank: Integer;
Begin
  Result := True;

  If fSize > 1 Then
  Begin
    PrevRank :=
      fStart;
    Rank :=
      IndexToRank(1);
    For
      I := 1 To fSize - 1 Do
    Begin
      If Comparator(fItems[Rank], fItems[PrevRank]) < 0 Then
      Begin
        Result :=
          False;
        Break;
      End;
      PrevRank := Rank;
      IncRank(Rank);
    End;
  End;
End;

{--- TGenDeque.DefaultItemToString ---}
Function TGenDeque.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenDeque.Iterate ---}
Procedure TGenDeque.Iterate(Process: TProcessItem);
Begin
  Iterate(
    Process, 0, fSize - 1);
End;

{--- TGenDeque.Iterate ---}
Procedure TGenDeque.Iterate(Process: TProcessItem; Const PosFrom, PosTo: Integer);
Var

  I, Rank: Integer;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  Rank :=
    IndexToRank(PosFrom);
  For
    I := PosFrom To PosTo Do
  Begin
    Process(
      fItems[Rank]);
    IncRank(Rank);
  End;
End;

{--- TGenDeque.LastItem ---}
Function TGenDeque.LastItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fItems[IndexToRank(fSize - 1)];
End;

{--- TGenDeque.MaxPos ---}
Function TGenDeque.MaxPos(PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    MaxPos(PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenDeque.MaxPos ---}
Function TGenDeque.MaxPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
Var

  I, IRank: Integer;
  Max: _TItem_;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo < PosFrom Then
  Begin
    I :=
      PosFrom;
    PosFrom :=
      PosTo;
    PosTo := I;
  End;

  Max :=
    fItems[IndexToRank(PosFrom)];
  Result :=
    PosFrom;
  IRank :=
    IndexToRank(PosFrom + 1);
  For
    I := PosFrom + 1 To PosTo Do
  Begin
    If Comparator(fItems[IRank], Max) > 0 Then
    Begin
      Result := I;
      Max :=
        fItems[IRank];
    End;
    IncRank(
      IRank);
  End;
End;

{--- TGenDeque.MaxPos ---}
Function TGenDeque.MaxPos: Integer;
Begin
  Result :=
    MaxPos(fOnCompareItems);
End;

{--- TGenDeque.MaxPos ---}
Function TGenDeque.MaxPos(Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    MaxPos(0, fSize - 1, Comparator);
End;

{--- TGenDeque.Merge ---}
Procedure TGenDeque.Merge(Src: TGenDeque);
Begin
  Merge(
    Src, fOnCompareItems);
End;

{--- TGenDeque.Merge ---}
Procedure TGenDeque.Merge(Src: TGenDeque; Comparator: TCompareItems);
Var

  A, B, C: Integer;
  ARank, BRank, CRank: Integer;
Begin
  If Src.fSize = 0 Then
    Exit;

  If fSize = 0 Then
    AppendAll(Src)
  Else
    If Comparator(Src.FirstItem, LastItem) >= 0 Then
      AppendAll(Src)
    Else
      If Comparator(FirstItem, Src.LastItem) >= 0 Then
        PrependAll(Src)
      Else
      Begin
        A :=
          fSize - 1;
        B :=
          Src.fSize - 1;

        InsertSpace(
          fSize, Src.fSize);
        C :=
          fSize - 1;

        ARank :=
          IndexToRank(A);
        BRank :=
          Src.IndexToRank(B);
        CRank :=
          IndexToRank(C);

        While
          C > 0 Do
        Begin
          If Comparator(fItems[ARank], Src.fItems[BRank]) > 0 Then
          Begin
            fItems
              [CRank] := fItems[ARank];
            Dec(A);
            If A < 0 Then
              Break;
            DecRank(
              ARank);
          End
          Else
          Begin
            fItems
              [CRank] := Src.fItems[BRank];
            Dec(B);
            If B < 0 Then
              Break;
            Src.
              DecRank(BRank);
          End;
          Dec(C);
          DecRank(
            CRank);
        End;

        If (C >= 0) And (B >= 0) Then
        Begin
          BRank :=
            Src.IndexToRank(B);
          ARank :=
            IndexToRank(B);
          While
            B >= 0 Do
          Begin
            fItems
              [ARank] := Src.fItems[BRank];
            Dec(B);
            DecRank(
              BRank);
            DecRank(
              ARank);
          End;
        End;

      End;
  Src.Clear;
End;

{--- TGenDeque.MinPos ---}
Function TGenDeque.MinPos(PosFrom, PosTo: Integer): Integer;
Begin
  Result :=
    MinPos(PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenDeque.MinPos ---}
Function TGenDeque.MinPos(PosFrom, PosTo: Integer; Comparator: TCompareItems): Integer;
Var

  I, IRank: Integer;
  Min: _TItem_;
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosTo < PosFrom Then
  Begin
    I :=
      PosFrom;
    PosFrom :=
      PosTo;
    PosTo := I;
  End;

  Result := -1;
  Min :=
    fItems[IndexToRank(PosFrom)];
  Result :=
    PosFrom;
  IRank :=
    IndexToRank(PosFrom + 1);
  For
    I := PosFrom + 1 To PosTo Do
  Begin
    If Comparator(fItems[IRank], Min) < 0 Then
    Begin
      Result := I;
      Min :=
        fItems[IRank];
    End;
    IncRank(
      IRank);
  End;
End;

{--- TGenDeque.MinPos ---}
Function TGenDeque.MinPos: Integer;
Begin
  Result :=
    MinPos(fOnCompareItems);
End;

{--- TGenDeque.MinPos ---}
Function TGenDeque.MinPos(Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    MinPos(0, fSize - 1, Comparator);
End;

{--- TGenDeque.Move ---}
Procedure TGenDeque.Move(Src, Dst, Count: Integer);
Var

  I: Integer;
Begin
  CheckIndex(Src);
  CheckIndex(Dst);

  If Src + Count > fSize Then
    Count :=
      fSize - Src;

  If Dst + Count > fSize Then
    Count :=
      fSize - Dst;

  If Count > 0 Then
  Begin
    If (Dst >= Src) And (Dst <= Src + Count - 1) Then
    Begin
      Dst :=
        Dst + Count - 1;
      Src :=
        Src + Count - 1;

      Dst :=
        IndexToRank(Dst);
      Src :=
        IndexToRank(Src);

      For
        I := 1 To Count Do
      Begin
        fItems
          [Dst] := fItems[Src];
        DecRank(Src);
        DecRank(Dst);
      End;
    End
    Else
    Begin
      Dst :=
        IndexToRank(Dst);
      Src :=
        IndexToRank(Src);

      For
        I := 1 To Count Do
      Begin
        fItems
          [Dst] := fItems[Src];
        IncRank(Src);
        IncRank(Dst);
      End;
    End;
  End;
End;

{--- TGenDeque.Prepend ---}
Procedure TGenDeque.Prepend(Const Item: _TItem_; Count: Integer);
Begin
  Insert(
    0, Item, Count);
End;

{--- TGenDeque.PrependAll ---}
Procedure TGenDeque.PrependAll(Src: TGenDeque);
Begin
  InsertAll(
    0, Src);
End;

{--- TGenDeque.PrependRange ---}
Procedure TGenDeque.PrependRange(Src: TGenDeque; PosFrom, PosTo: Integer);
Begin
  InsertRange(
    0, Src, PosFrom, PosTo);
End;

{--- TGenDeque.ReadFirstItem ---}
Procedure TGenDeque.ReadFirstItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fItems[fStart];
End;

{--- TGenDeque.ReadItem ---}
Procedure TGenDeque.ReadItem(Position: Integer; out Value: _TItem_);
Begin
  CheckIndex(
    Position);
  Value :=
    fItems[IndexToRank(Position)];
End;

{--- TGenDeque.ReadItemFast ---}
Procedure TGenDeque.ReadItemFast(Position: Integer; out Value: _TItem_);
Begin
  Value :=
    fItems[IndexToRank(Position)];
End;

{--- TGenDeque.ReadLastItem ---}
Procedure TGenDeque.ReadLastItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fItems[IndexToRank(fSize - 1)];
End;

{--- TGenDeque.Sort ---}
Procedure TGenDeque.Sort(PosFrom, PosTo: Integer);
Begin
  Sort(
    PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenDeque.Sort ---}
Procedure TGenDeque.Sort(PosFrom, PosTo: Integer; Comparator: TCompareItems);
Begin
  CheckIndex(
    PosFrom);
  CheckIndex(
    PosTo);

  If PosFrom >= PosTo Then
    Exit;

  Quicksort(
    PosFrom, PosTo, Comparator);
End;

{--- TGenDeque.Sort ---}
Procedure TGenDeque.Sort;
Begin
  Sort(
    fOnCompareItems);
End;

{--- TGenDeque.Sort ---}
Procedure TGenDeque.Sort(Comparator: TCompareItems);
Begin
  If fSize > 1 Then
    Sort(
      0, fSize - 1, Comparator);
End;

{--- TGenDeque.RealMoveRank ---}
Procedure TGenDeque.RealMoveRank(Src, Dst, Count: Integer);
Var

  SrcLast, I, DstCurrent: Integer;
Begin
  If Count <= 0 Then
    Exit;

  SrcLast :=
    Src + Count - 1;
  If (Dst >= Src) And (Dst <= SrcLast) Then
  Begin
    DstCurrent :=
      Dst + Count - 1;
    For
      I := SrcLast Downto Src Do
    Begin
      fItems
        [DstCurrent] := fItems[I];
      Dec(
        DstCurrent);
    End;
  End
  Else
  Begin
    DstCurrent := Dst;
    For
      I := Src To SrcLast Do
    Begin
      fItems
        [DstCurrent] := fItems[I];
      Inc(
        DstCurrent);
    End;
  End;
End;

{--- TGenDeque.RealMoveIndex ---}
Class
Procedure TGenDeque.RealMoveIndex(Src, Dst: TGenDeque;
  SrcFirst, DstFirst, Count: Integer);
Var

  SrcLast, I, DstCurrent: Integer;
Begin
  SrcLast :=
    SrcFirst + Count - 1;
  If (Src = Dst) And ((DstFirst >= SrcFirst) And (DstFirst <= SrcLast)) Then
  Begin
    DstCurrent :=
      DstFirst + Count - 1;
    For
      I := SrcLast Downto SrcFirst Do
    Begin
      Dst
        [DstCurrent] := Src[I];
      Dec(
        DstCurrent);
    End;
  End
  Else
  Begin
    DstCurrent :=
      DstFirst;
    For
      I := SrcFirst To SrcLast Do
    Begin
      Dst
        [DstCurrent] := Src[I];
      Inc(
        DstCurrent);
    End;
  End;
End;

{--- TGenDeque.ReduceCapacity ---}
Procedure TGenDeque.ReduceCapacity(ACapacity: Integer);
Var

  NewStart, ItemsAtEnd: Integer;
Begin
  If fStart + fSize >= fCapacity Then
  Begin
    ItemsAtEnd :=
      fCapacity - fStart;
    NewStart :=
      ACapacity - ItemsAtEnd;
    RealMoveRank(
      fStart, NewStart, ItemsAtEnd);
    fStart :=
      NewStart;
  End;

  SetLength(
    fItems, ACapacity);
  fCapacity :=
    ACapacity;
End;

{--- TGenDeque.Replace ---}
Procedure TGenDeque.Replace(Index, Count: Integer; Const Value: _TItem_);
Begin
  CheckIndex(
    Index);

  If Count > 0 Then
  Begin
    If Index + Count >= fSize Then
      Count :=
        fSize - Index;
    Fill(
      Index, Count, Value);
  End;
End;

{--- TGenDeque.ReverseFindIndex ---}
Function TGenDeque.ReverseFindIndex(Const Item: _TItem_): Integer;
Begin
  Result :=
    ReverseFindIndex(Item, fOnCompareItems);
End;

{--- TGenDeque.ReverseFindIndex ---}
Function TGenDeque.ReverseFindIndex(Const Item: _TItem_;
  Comparator: TCompareItems): Integer;
Begin
  If fSize = 0 Then
    Result := -1
  Else
    Result :=
      ReverseFindIndex(Item, fSize - 1, Comparator);
End;

{--- TGenDeque.ReverseFindIndex ---}
Function TGenDeque.ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer): Integer;
Begin
  Result :=
    ReverseFindIndex(Item, PosFrom, fOnCompareItems);
End;

{--- TGenDeque.ReverseFindIndex ---}
Function TGenDeque.ReverseFindIndex(Const Item: _TItem_; PosFrom: Integer;
  Comparator: TCompareItems): Integer;
Var

  I, Pos: Integer;
Begin
  CheckIndex(
    PosFrom);

  Result := -1;
  Pos :=
    IndexToRank(PosFrom);
  For
    I := PosFrom Downto 0 Do
  Begin
    If Comparator(fItems[Pos], Item) = 0 Then
    Begin
      Result := I;
      Break;
    End;
    DecRank(Pos);
  End;
End;

{--- TGenDeque.SetCapacity ---}
Procedure TGenDeque.SetCapacity(ACapacity: Integer);
Begin
  If ACapacity <= fCapacity Then
    ReduceCapacity(
      ACapacity)
  Else
    If ACapacity > fCapacity Then
      IncreaseCapacity(
        ACapacity);
End;

{--- TGenDeque.SetOnCompareItems ---}
Procedure TGenDeque.SetOnCompareItems(AValue: TCompareItems);
Begin
  If AValue = nil Then
    fOnCompareItems := @DefaultCompareItems
  Else
    fOnCompareItems :=
      AValue;
End;

{--- TGenDeque.SetOnItemToString ---}
Procedure TGenDeque.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fOnItemToString := @DefaultItemToString
  Else
    fOnItemToString :=
      AValue;
End;

{--- TGenDeque.SetItem ---}
Procedure TGenDeque.SetItem(Position: Integer; Const Value: _TItem_);
Begin
  CheckIndex(
    Position);
  fItems
    [
    IndexToRank(Position)] := Value;
End;

{--- TGenDeque.SetItemFast ---}
Procedure TGenDeque.SetItemFast(Position: Integer; Const Value: _TItem_);
Begin
  fItems
    [
    IndexToRank(Position)] := Value;
End;

{--- TGenDeque.SwapFast ---}
Procedure TGenDeque.SwapFast(I, J: Integer);
Var

  Temp: _TItem_;
Begin
  I :=
    IndexToRank(I);
  J :=
    IndexToRank(J);

  Temp :=
    fItems[I];
  fItems
    [I] := fItems[J];
  fItems
    [J] := Temp;
End;

{===================}
{=== TListCursor ===}
{===================}

{--- TListCursor.Equals ---}
Function TListCursor.Equals(Const Cursor: TListCursor): Boolean;
Begin
  Result :=
    (fList = Cursor.fList) And (fNode = Cursor.fNode);
End;

{--- TListCursor.HasItem ---}
Function TListCursor.HasItem: Boolean;
Begin
  Result :=
    (fNode <> nil);
End;

{--- TListCursor.Init ---}
Constructor TListCursor.Init(AList: TAbstractList; ANode: Pointer);
Begin
  fList :=
    AList;
  fNode :=
    ANode;
End;

{--- TListCursor.IsFirst ---}
Function TListCursor.IsFirst: Boolean;
Begin
  Result :=
    fList.CursorIsFirst(Self);
End;

{--- TListCursor.IsLast ---}
Function TListCursor.IsLast: Boolean;
Begin
  Result :=
    fList.CursorIsLast(Self);
End;

{--- TListCursor.IsNil ---}
Function TListCursor.IsNil: Boolean;
Begin
  Result :=
    (fNode = nil);
End;

{--- TListCursor.MoveNext ---}
Procedure TListCursor.MoveNext;
Begin
  fList.
    CursorMoveNext(Self);
End;

{--- TListCursor.MovePrevious ---}
Procedure TListCursor.MovePrevious;
Begin
  fList.
    CursorMovePrev(Self);
End;

{=====================}
{=== TAbstractList ===}
{=====================}

{--- TAbstractList.CheckValid ---}
Procedure TAbstractList.CheckValid(Const Cursor: TListCursor);
Begin
  If Cursor.List <> Self Then
    RaiseCursorDenotesWrongContainer;
End;

{--- TAbstractList.CheckNotNil ---}
Procedure TAbstractList.CheckNotNil(Const Cursor: TListCursor);
Begin
  CheckValid(
    Cursor);
  If Cursor.IsNil Then
    RaiseCursorIsNil;
End;

{================}
{=== TGenList ===}
{================}

{--- TGenList.Append ---}
Procedure TGenList.Append(Const Item: _TItem_; Count: Integer);
Begin
  Insert(
    fNilCursor, Item, Count);
End;

{--- TGenList.AppendAll ---}
Procedure TGenList.AppendAll(Src: TGenList);
Begin
  InsertAll(
    fNilCursor, Src);
End;

{--- TGenList.AppendRange ---}
Procedure TGenList.AppendRange(Src: TGenList; Const PosFrom, PosTo: TListCursor);
Begin
  InsertRange(
    fNilCursor, Src, PosFrom, PosTo);
End;

{--- TGenList.Clear ---}
Procedure TGenList.Clear;
Begin
  DeleteFirst(
    fSize);
End;

{--- TGenList.DefaultCompareItems ---}
Function TGenList.DefaultCompareItems(Const A, B: _TItem_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenList.Contains ---}
Function TGenList.Contains(Const Item: _TItem_): Boolean;
Begin
  Result :=
    Contains(Item, fOnCompareItems);
End;

{--- TGenList.Contains ---}
Function TGenList.Contains(Const Item: _TItem_; Comparator: TCompareItems): Boolean;
Begin
  Result := Not Find(Item, Comparator).IsNil;
End;

{--- TGenList.Create ---}
Constructor TGenList.Create;
Begin
  Inherited
  Create;

  New(
    fHead);
  New(
    fTail);
  fHead  ^.Next := fTail;
  fTail  ^.Previous := fHead;

  fNilCursor.
    Init(Self, nil);

  SetOnCompareItems(nil);
  SetOnItemToString(nil);
End;

{--- TGenList.Delete ---}
Procedure TGenList.Delete(Var Position: TListCursor; Count: Integer);
Begin
  CheckNotNil(
    Position);
  DeleteNodesForward(
    PNode(Position.Node), Count);
  Position :=
    fNilCursor;
End;

{--- TGenList.DeleteFirst ---}
Procedure TGenList.DeleteFirst(Count: Integer);
Begin
  If (fSize > 0) And (Count > 0) Then
    DeleteNodesForward(
      fHead^.Next, Count);
End;

{--- TGenList.DeleteLast ---}
Procedure TGenList.DeleteLast(Count: Integer);
Begin
  If (fSize > 0) And (Count > 0) Then
    DeleteNodesBackward(
      fTail^.Previous, Count);
End;

{--- TGenList.DeleteNodesBackward ---}
Procedure TGenList.DeleteNodesBackward(From: PNode; Count: Integer);
Var

  Current, AfterFrom: PNode;
Begin
  AfterFrom :=
    From^.Next;

  Current := From;
  While
    (Count > 0) And (Current <> fHead) Do
  Begin
    Current  ^.Previous^.Next := AfterFrom;
    AfterFrom  ^.Previous := Current^.Previous;

    Dispose(
      Current);
    Dec(
      fSize);
    Dec(
      Count);
    Current :=
      AfterFrom^.Previous;
  End;
End;

{--- TGenList.DeleteNodesBetween ---}
Procedure TGenList.DeleteNodesBetween(NodeFrom, NodeTo: PNode);
Var

  Current, Previous, Limit: PNode;
Begin
  Current :=
    NodeFrom;
  Previous :=
    Current^.Previous;
  Limit :=
    NodeTo^.Next;

  While
    Current <> Limit Do
  Begin
    Previous  ^.Next := Current^.Next;
    Current  ^.Next^.Previous := Previous;

    Dispose(
      Current);
    Dec(
      fSize);
    Current :=
      Previous^.Next;
  End;
End;

{--- TGenList.DeleteNodesForward ---}
Procedure TGenList.DeleteNodesForward(From: PNode; Count: Integer);
Var

  Current, BeforeFrom: PNode;
Begin
  BeforeFrom :=
    From^.Previous;
  Current := From;
  While
    (Count > 0) And (Current <> fTail) Do
  Begin
    BeforeFrom  ^.Next := Current^.Next;
    Current  ^.Next^.Previous := BeforeFrom;

    Dispose(
      Current);
    Dec(
      fSize);
    Dec(
      Count);
    Current :=
      BeforeFrom^.Next;
  End;
End;

{--- TGenList.EnumeratorGet ---}
Function TGenList.EnumeratorGet(Const Pos: TListCursor): _TItem_;
Begin
  ReadItemFast(
    Pos, Result);
End;

{--- TGenList.EnumeratorNext ---}
Function TGenList.EnumeratorNext(Var Pos: TListCursor): Boolean;
Begin
  If Pos.IsNil Then
    Pos :=
      First
  Else
    Pos.MoveNext;
  Result :=
    Pos.HasItem;
End;

{--- TGenList.DeleteRange ---}
Procedure TGenList.DeleteRange(Const PosFrom, PosTo: TListCursor);
Begin
  CheckNotNil(
    PosFrom);
  CheckNotNil(
    PosTo);
  DeleteNodesBetween(
    PosFrom.Node, PosTo.Node);
End;

{--- TGenList.Destroy ---}
Destructor TGenList.Destroy;
Begin
  Clear;
  Dispose(
    fHead);
  Dispose(
    fTail);
  Inherited
  Destroy;
End;

{--- TGenList.Equals ---}
Function TGenList.Equals(List: TGenList; Comparator: TCompareItems): Boolean;
Var

  N1, N2: PNode;
Begin
  If fSize <> List.fSize Then
  Begin
    Result :=
      False;
    Exit;
  End;

  Result := True;
  N1 :=
    fHead^.Next;
  N2 :=
    List.fHead^.Next;

  While
    N1 <> fTail Do
  Begin
    If Comparator(N1^.Item, N2^.Item) <> 0 Then
    Begin
      Result :=
        False;
      Break;
    End;
    N1 :=
      N1^.Next;
    N2 :=
      N2^.Next;
  End;
End;

{--- TGenList.Equals ---}
Function TGenList.Equals(Obj: TObject): Boolean;
Begin
  Result :=
    Equals(Obj, fOnCompareItems);
End;

{--- TGenList.Equals ---}
Function TGenList.Equals(Obj: TObject; Comparator: TCompareItems): Boolean;
Begin
  If Obj = Self Then
    Result := True
  Else
    If Obj Is TGenList Then
      Result :=
        Equals(Obj As TGenList, Comparator)
    Else
      Result :=
        False;
End;

{--- TGenList.Find ---}
Function TGenList.Find(Const Item: _TItem_): TListCursor;
Begin
  Result :=
    Find(Item, fOnCompareItems);
End;

{--- TGenList.Find ---}
Function TGenList.Find(Const Item: _TItem_; Comparator: TCompareItems): TListCursor;
Begin
  If fSize = 0 Then
    Result :=
      fNilCursor
  Else
    Result :=
      Find(Item, First, Comparator);
End;

{--- TGenList.Find ---}
Function TGenList.Find(Const Item: _TItem_; Const Position: TListCursor): TListCursor;
Begin
  Result :=
    Find(Item, Position, fOnCompareItems);
End;

{--- TGenList.Find ---}
Function TGenList.Find(Const Item: _TItem_; Const Position: TListCursor;
  Comparator: TCompareItems): TListCursor;
Var

  Node: PNode;
  I: _TItem_;
Begin
  CheckValid(
    Position);

  If Position.IsNil Then
    Node :=
      fHead^.Next
  Else
    Node :=
      Position.Node;

  While
    Node <> fTail Do
  Begin
    I :=
      Node^.Item;
    If Comparator(Item, I) = 0 Then
      Break;
    Node :=
      Node^.Next;
  End;

  If (Node = fTail) Or (Node = fHead) Then
    Node := nil;

  Result.
    Init(Self, Node);
End;

{--- TGenList.First ---}
Function TGenList.First: TListCursor;
Begin
  If fSize > 0 Then
    Result.
      Init(Self, fHead^.Next)
  Else
    Result :=
      fNilCursor;
End;

{--- TGenList.FirstItem ---}
Function TGenList.FirstItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fHead^.Next^.Item;
End;

{--- TGenList.GetCursor ---}
Function TGenList.GetCursor(Index: Integer): TListCursor;
Var

  DistanceFromHead, DistanceFromTail: Longint;
  Node: PNode;
Begin
  If (Index < -1) Or (Index > fSize) Then
    Result :=
      fNilCursor
  Else
  Begin
    DistanceFromHead :=
      Index + 1;
    DistanceFromTail :=
      fSize - Index;

    If DistanceFromHead < DistanceFromTail Then
    Begin
      Node :=
        fHead;
      While
        DistanceFromHead > 0 Do
      Begin
        Node :=
          Node^.Next;
        Dec(
          DistanceFromHead);
      End;
    End
    Else
    Begin
      Node :=
        fTail;
      While
        DistanceFromTail > 0 Do
      Begin
        Node :=
          Node^.Previous;
        Dec(
          DistanceFromTail);
      End;
    End;

    Result.
      Init(Self, Node);
  End;
End;

{--- TGenList.GetEnumerator ---}
Function TGenList.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenList.GetItem ---}
Function TGenList.GetItem(Const Position: TListCursor): _TItem_;
Begin
  CheckNotNil(
    Position);
  Result :=
    PNode(Position.Node)^.Item;
End;

{--- TGenList.GetItemFast ---}
Function TGenList.GetItemFast(Const Position: TListCursor): _TItem_;
Begin
  Result :=
    PNode(Position.Node)^.Item;
End;

{--- TGenList.GetItemFast ---}
Function TGenList.GetItemPtr(Const Position: TListCursor): PItem;
Begin
  CheckNotNil(
    Position);
  Result := @PNode(Position.Node)^.Item;
End;

{--- TGenList.GetItemFast ---}
Function TGenList.GetItemPtrFast(Const Position: TListCursor): PItem;
Begin
  Result := @PNode(Position.Node)^.Item;
End;

{--- TGenList.Insert ---}
Procedure TGenList.Insert(Const Before: TListCursor; Const Item: _TItem_;
  Count: Integer);
Var

  BeforeNode: PNode;
Begin
  CheckValid(
    Before);

  If Before.HasItem Then
    BeforeNode :=
      PNode(Before.Node)
  Else
    BeforeNode :=
      fTail;

  InsertItem(
    Item, BeforeNode, Count);
End;

{--- TGenList.Insert ---}
Procedure TGenList.Insert(Const Before: TListCursor; Const Item: _TItem_;
  out Position: TListCursor; Count: Integer);
Var

  Prev, BeforeNode: PNode;
Begin
  CheckValid(
    Before);

  If Before.HasItem Then
    BeforeNode :=
      PNode(Before.Node)
  Else
    BeforeNode :=
      fTail;

  Prev :=
    BeforeNode^.Previous;

  InsertItem(
    Item, BeforeNode, Count);

  Position.
    Init(Self, Prev^.Next);
End;

{--- TGenList.InsertAll ---}
Procedure TGenList.InsertAll(Const Before: TListCursor; Src: TGenList);
Begin
  If Src.fSize > 0 Then
    InsertRange(
      Before, Src, Src.First, Src.Last);
End;

{--- TGenList.InsertItem ---}
Procedure TGenList.InsertItem(Const Item: _TItem_; Pos: PNode; Count: Integer);
Var

  Node: PNode;
Begin
  While
    Count > 0 Do
  Begin
    New(
      Node);
    Node  ^.Item := Item;

    Pos  ^.Previous^.Next := Node;

    Node  ^.Previous := Pos^.Previous;
    Node  ^.Next := Pos;

    Pos  ^.Previous := Node;

    Inc(
      fSize);
    Dec(
      Count);
  End;
End;

{--- TGenList.InsertRange ---}
Procedure TGenList.InsertRange(Const Before: TListCursor; Src: TGenList;
  Const PosFrom, PosTo: TListCursor);
Var

  Copy: TGenList;
  Node, LastNode: PNode;
Begin
  CheckValid(
    Before);
  Src.
    CheckNotNil(PosFrom);
  Src.
    CheckNotNil(PosTo);

  Copy :=
    TGenList.Create;
  Try
    Node :=
      PNode(PosFrom.Node);
    LastNode :=
      PNode(PosTo.Node)^.Next;

    While
      Node <> LastNode Do
    Begin
      Copy.
        Append(Node^.Item);
      Node :=
        Node^.Next;
    End;

    Splice(
      Before, Copy);
  Finally
    Copy.Free;
  End;
End;

{--- TGenList.IsEmpty ---}
Function TGenList.IsEmpty: Boolean;
Begin
  Result :=
    (fSize = 0);
End;

{--- TGenList.IsSorted ---}
Function TGenList.IsSorted: Boolean;
Begin
  Result :=
    IsSorted(fOnCompareItems);
End;

{--- TGenList.IsSorted ---}
Function TGenList.IsSorted(Comparator: TCompareItems): Boolean;
Var

  N: PNode;
  I: Integer;
Begin
  Result := True;

  N :=
    fHead^.Next;
  For
    I := 2 To fSize Do
  Begin
    If Comparator(N^.Item, N^.Next^.Item) > 0 Then
    Begin
      Result :=
        False;
      Break;
    End;
    N :=
      N^.Next;
  End;
End;

{--- TGenList.DefaultItemToString ---}
Function TGenList.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenList.Iterate ---}
Procedure TGenList.Iterate(Process: TProcessItem);
Begin
  If fSize > 0 Then
    Iterate(
      Process, First, Last);
End;

{--- TGenList.Iterate ---}
Procedure TGenList.Iterate(Process: TProcessItem; Const PosFrom, PosTo: TListCursor);
Var

  Node, Limit: PNode;
Begin
  CheckNotNil(
    PosFrom);
  CheckNotNil(
    PosTo);

  Node :=
    PNode(PosFrom.Node);
  Limit :=
    PNode(PosTo.Node)^.Next;

  While
    Node <> Limit Do
  Begin
    Process(
      Node^.Item);
    Node :=
      Node^.Next;
  End;
End;

{--- TGenList.Last ---}
Function TGenList.Last: TListCursor;
Begin
  If fSize > 0 Then
    Result.
      Init(Self, fTail^.Previous)
  Else
    Result.
      Init(Self, nil);
End;

{--- TGenList.LastItem ---}
Function TGenList.LastItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fTail^.Previous^.Item;
End;

{--- TGenList.Merge ---}
Procedure TGenList.Merge(Src: TGenList);
Begin
  Merge(
    Src, fOnCompareItems);
End;

{--- TGenList.Merge ---}
Procedure TGenList.Merge(Src: TGenList; Comparator: TCompareItems);
Var

  Node, SrcNode, N: PNode;
Begin
  If Src = Self Then
    Exit;

  Node :=
    fHead^.Next;
  SrcNode :=
    Src.fHead^.Next;

  While
    SrcNode <> Src.fTail Do
  Begin
    If Node = fTail Then
    Begin
      SpliceNodes(
        fTail, SrcNode, SrcNode);
      fSize :=
        fSize + Src.fSize;
      Src.fSize := 0;
      Break;
    End;

    If Comparator(SrcNode^.Item, Node^.Item) < 0 Then
    Begin
      N :=
        SrcNode^.Next;
      SpliceNodes(
        Node, SrcNode, SrcNode);
      Dec(
        Src.fSize);
      Inc(
        fSize);
      SrcNode := N;
    End
    Else
      Node :=
        Node^.Next;
  End;
End;

{--- TGenList.Partition ---}
Procedure TGenList.Partition(Pivot, Back: PNode; Comparator: TCompareItems);
Var

  Node, Next: PNode;
Begin
  Node :=
    Pivot^.Next;
  While
    Node <> Back Do
    If Comparator(Node^.Item, Pivot^.Item) < 0 Then
    Begin
      Next :=
        Node^.Next;
      SpliceNodes(
        Pivot, Node, Node);
      Node := Next;
    End
    Else
      Node :=
        Node^.Next;
End;

{--- TGenList.Prepend ---}
Procedure TGenList.Prepend(Const Item: _TItem_; Count: Integer);
Begin
  Insert(
    First, Item, Count);
End;

{--- TGenList.PrependAll ---}
Procedure TGenList.PrependAll(Src: TGenList);
Begin
  InsertAll(
    First, Src);
End;

{--- TGenList.PrependRange ---}
Procedure TGenList.PrependRange(Src: TGenList; Const PosFrom, PosTo: TListCursor);
Begin
  InsertRange(
    First, Src, PosFrom, PosTo);
End;

{--- TGenList.ReadFirstItem ---}
Procedure TGenList.ReadFirstItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fHead^.Next^.Item;
End;

{--- TGenList.ReadItem ---}
Procedure TGenList.ReadItem(Const Position: TListCursor; out Value: _TItem_);
Begin
  CheckNotNil(
    Position);
  Value :=
    PNode(Position.Node)^.Item;
End;

{--- TGenList.ReadItemFast ---}
Procedure TGenList.ReadItemFast(Const Position: TListCursor; out Value: _TItem_);
Begin
  Value :=
    PNode(Position.Node)^.Item;
End;

{--- TGenList.ReadLastItem ---}
Procedure TGenList.ReadLastItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fTail^.Previous^.Item;
End;

{--- TGenList.RealSort ---}
Procedure TGenList.RealSort(Front, Back: PNode; Comparator: TCompareItems);
Var

  Pivot: PNode;
Begin
  Pivot :=
    Front^.Next;
  If Pivot <> Back Then
  Begin
    Partition(
      Pivot, Back, Comparator);
    RealSort(
      Front, Pivot, Comparator);
    RealSort(
      Pivot, Back, Comparator);
  End;
End;

{--- TGenList.SetOnCompareItems ---}
Procedure TGenList.SetOnCompareItems(AValue: TCompareItems);
Begin
  If AValue = nil Then
    fOnCompareItems := @DefaultCompareItems
  Else
    fOnCompareItems :=
      AValue;
End;

{--- TGenList.SetOnItemToString ---}
Procedure TGenList.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fOnItemToString := @DefaultItemToString
  Else
    fOnItemToString :=
      AValue;
End;

{--- TGenList.Replace ---}
Procedure TGenList.Replace(Const Position: TListCursor; Count: Integer;
  Const Value: _TItem_);
Var

  Node: PNode;
Begin
  CheckNotNil(
    Position);

  Node :=
    PNode(Position.Node);
  While
    (Count > 0) And (Node <> fTail) Do
  Begin
    Node  ^.Item := Value;
    Dec(
      Count);
    Node :=
      Node^.Next;
  End;
End;

{--- TGenList.Reverse ---}
Procedure TGenList.Reverse;
Begin
  If fSize > 1 Then
    ReverseRange(
      First, Last);
End;

{--- TGenList.ReverseFind ---}
Function TGenList.ReverseFind(Const Item: _TItem_): TListCursor;
Begin
  Result :=
    ReverseFind(Item, fOnCompareItems);
End;

{--- TGenList.ReverseFind ---}
Function TGenList.ReverseFind(Const Item: _TItem_;
  Comparator: TCompareItems): TListCursor;
Begin
  If fSize = 0 Then
    Result :=
      fNilCursor
  Else
    Result :=
      ReverseFind(Item, Last, Comparator);
End;

{--- TGenList.ReverseFind ---}
Function TGenList.ReverseFind(Const Item: _TItem_;
  Const Position: TListCursor): TListCursor;
Begin
  Result :=
    ReverseFind(Item, Position, fOnCompareItems);
End;

{--- TGenList.ReverseFind ---}
Function TGenList.ReverseFind(Const Item: _TItem_; Const Position: TListCursor;
  Comparator: TCompareItems): TListCursor;
Var

  Node: PNode;
  I: _TItem_;
Begin
  CheckValid(
    Position);

  If Position.IsNil Then
    Node :=
      fTail^.Previous
  Else
    Node :=
      PNode(Position.Node);

  If Node = fTail Then
    Node :=
      Node^.Previous;

  While
    Node <> fHead Do
  Begin
    I :=
      Node^.Item;
    If Comparator(Item, I) = 0 Then
      Break;
    Node :=
      Node^.Previous;
  End;

  If (Node = fTail) Or (Node = fHead) Then
    Node := nil;

  Result.
    Init(Self, Node);
End;

{--- TGenList.ReverseRange ---}
Procedure TGenList.ReverseRange(Const PosFrom, PosTo: TListCursor);
Var

  Left, Right: PNode;
  Tmp: _TItem_;
Begin
  CheckNotNil(
    PosFrom);
  CheckNotNil(
    PosTo);

  If Not PosFrom.Equals(PosTo) Then
  Begin
    Left :=
      PNode(PosFrom.Node);
    Right :=
      PNode(PosTo.Node);
    While
      True Do
    Begin
      Tmp :=
        Left^.Item;
      Left  ^.Item := Right^.Item;
      Right  ^.Item := Tmp;

      Left :=
        Left^.Next;
      If Left = Right Then
        Break;

      Right :=
        Right^.Previous;
      If Left = Right Then
        Break;
    End;
  End;
End;

{--- TGenList.SetItem ---}
Procedure TGenList.SetItem(Const Position: TListCursor; Const Value: _TItem_);
Begin
  CheckNotNil(
    Position);
  PNode(
    Position.Node)^.Item := Value;
End;

{--- TGenList.SetItemFast ---}
Procedure TGenList.SetItemFast(Const Position: TListCursor; Const Value: _TItem_);
Begin
  PNode(
    Position.Node)^.Item := Value;
End;

{--- TGenList.Sort ---}
Procedure TGenList.Sort(Const PosFrom, PosTo: TListCursor);
Begin
  Sort(
    PosFrom, PosTo, fOnCompareItems);
End;

{--- TGenList.Sort ---}
Procedure TGenList.Sort(Const PosFrom, PosTo: TListCursor; Comparator: TCompareItems);
Begin
  CheckNotNil(
    PosFrom);
  CheckNotNil(
    PosTo);
  RealSort(
    PNode(PosFrom.Node)^.Previous, PNode(PosTo.Node)^.Next,
    Comparator);
End;

{--- TGenList.Sort ---}
Procedure TGenList.Sort;
Begin
  Sort(
    fOnCompareItems);
End;

{--- TGenList.Sort ---}
Procedure TGenList.Sort(Comparator: TCompareItems);
Begin
  If fSize > 1 Then
    Sort(
      First, Last, Comparator);
End;

{--- TGenList.Splice ---}
Procedure TGenList.Splice(Const Before: TListCursor; Src: TGenList);
Var

  Where: PNode;
Begin
  CheckValid(
    Before);

  If (Self <> Src) And (Src.fSize > 0) Then
  Begin
    If Before.IsNil Then
      Where := fTail
    Else
      Where :=
        PNode(Before.Node);

    SpliceNodes(
      Where, Src.fHead^.Next, Src.fTail^.Previous);
    Inc(
      fSize, Src.fSize);
    Src.fSize := 0;
  End;
End;

{--- TGenList.Splice ---}
Procedure TGenList.Splice(Const Before: TListCursor; Src: TGenList;
  Const SrcFrom, SrcTo: TListCursor);
Var

  Node, Where: PNode;
  Count: Integer = 0;
Begin
  CheckValid(
    Before);
  Src.
    CheckNotNil(SrcFrom);
  Src.
    CheckNotNil(SrcTo);

  If (Src = Self) And Before.HasItem Then
  Begin
    If Before.Equals(SrcFrom) Or Before.Equals(SrcTo) Then
      RaiseError(
        'cursor `Before'' is in range [SrcFrom..SrcTo]');

    Node :=
      PNode(SrcFrom.Node)^.Next;
    While
      Node <> PNode(SrcTo.Node) Do
    Begin
      If Node = PNode(Before.Node) Then
        RaiseError(

          'cursor `Before'' is in range [SrcFrom..SrcTo]'
          );
      Node :=
        Node^.Next;
    End;
  End
  Else
    If Src <> Self Then
    Begin
      Node :=
        PNode(SrcFrom.Node);
      While
        Node <> PNode(SrcTo.Node) Do
      Begin
        Node :=
          Node^.Next;
        Inc(
          Count);
      End;
      Inc(
        Count);
    End;

  If Before.HasItem Then
    Where :=
      PNode(Before.Node)
  Else
    Where :=
      fTail;

  SpliceNodes(
    Where, PNode(SrcFrom.Node), PNode(SrcTo.Node));
  Inc(
    fSize, Count);
  Dec(
    Src.fSize, Count);
End;

{--- TGenList.Splice ---}
Procedure TGenList.Splice(Const Before: TListCursor; Src: TGenList;
  Const Position: TListCursor);
Var

  Where: PNode;
Begin
  CheckValid(
    Before);
  Src.
    CheckNotNil(Position);

  If Not Position.Equals(Before) Then
  Begin
    If Before.HasItem Then
      Where :=
        PNode(Before.Node)
    Else
      Where :=
        fTail;

    SpliceNodes(
      Where, PNode(Position.Node), PNode(Position.Node));
    Inc(
      fSize);
    Dec(
      Src.fSize);
  End;
End;

{--- TGenList.SpliceNodes ---}
Procedure TGenList.SpliceNodes(Before, PosFrom, PosTo: PNode);
Begin
  PosFrom  ^.Previous^.Next := PosTo^.Next;
  PosTo  ^.Next^.Previous := PosFrom^.Previous;

  Before  ^.Previous^.Next := PosFrom;
  PosFrom  ^.Previous := Before^.Previous;

  PosTo  ^.Next := Before;
  Before  ^.Previous := PosTo;
End;

{--- TGenList.CursorIsFirst ---}
Function TGenList.CursorIsFirst(Const Cursor: TListCursor): Boolean;
Begin
  Result :=
    (PNode(Cursor.Node) = (Cursor.List As TGenList).fHead^.Next) And
    (PNode(Cursor.Node) <> (Cursor.List As TGenList).fTail);
End;

{--- TGenList.CursorIsLast ---}
Function TGenList.CursorIsLast(Const Cursor: TListCursor): Boolean;
Begin
  Result :=
    (PNode(Cursor.Node) = (Cursor.List As TGenList).fTail^.Previous) And
    (PNode(Cursor.Node) <> (Cursor.List As TGenList).fHead);
End;

{--- TGenList.CursorMoveNext ---}
Procedure TGenList.CursorMoveNext(Var Cursor: TListCursor);
Begin
  If Cursor.Node <> nil Then
  Begin
    Cursor.Node := PNode(Cursor.Node)^.Next;
    If PNode(Cursor.Node) = (Cursor.List As TGenList).fTail Then
      Cursor.Node := nil;
  End;
End;

{--- TGenList.CursorMovePrev ---}
Procedure TGenList.CursorMovePrev(Var Cursor: TListCursor);
Begin
  If Cursor.Node <> nil Then
  Begin
    Cursor.Node := PNode(Cursor.Node)^.Previous;
    If PNode(Cursor.Node) = (Cursor.List As TGenList).fHead Then
      Cursor.Node := nil;
  End;
End;

{--- TGenList.Swap ---}
Procedure TGenList.Swap(Const I, J: TListCursor);
Var

  Tmp: _TItem_;
Begin
  CheckNotNil(I);
  CheckNotNil(J);

  If I.Node <> J.Node Then
  Begin
    Tmp :=
      PNode(I.Node)^.Item;
    PNode(
      I.Node)^.Item := PNode(J.Node)^.Item;
    PNode(
      J.Node)^.Item := Tmp;
  End;
End;

{--- TGenList.SwapLinks ---}
Procedure TGenList.SwapLinks(Const I, J: TListCursor);
Var

  NextI: PNode;
Begin
  CheckNotNil(I);
  CheckNotNil(J);

  If I.Node <> J.Node Then
  Begin
    NextI :=
      PNode(I.Node)^.Next;

    If NextI = PNode(J.Node) Then
      SpliceNodes(
        PNode(I.Node), PNode(J.Node), PNode(J.Node))
    Else
    Begin
      SpliceNodes(
        PNode(J.Node), PNode(I.Node), PNode(I.Node));
      SpliceNodes(
        NextI, PNode(J.Node), PNode(J.Node));
    End;
  End;
End;

{--- TGenList.ToString ---}
Function TGenList.ToString: String;
Var

  Node: PNode;
Begin
  Result := '(';

  If fSize > 0 Then
  Begin
    Node :=
      fHead^.Next;
    While
      Node <> fTail Do
    Begin
      Result :=
        Result + fOnItemToString(Node^.Item) + ', ';
      Node :=
        Node^.Next;
    End;
    SetLength(
      Result, Length(Result) - 2);
  End;

  Result :=
    Result + ')';
End;

{=========================}
{=== TGenPriorityQueue ===}
{=========================}

{--- TGenPriorityQueue.Clear ---}
Procedure TGenPriorityQueue.Clear;
Begin
  SetLength(
    fItems, 1);
  fCapacity := 1;
  fSize := 0;
End;

{--- TGenPriorityQueue.Create ---}
Constructor TGenPriorityQueue.Create(InitialCapacity: Integer);
Begin
  Inherited
  Create;

  If InitialCapacity < 1 Then
    InitialCapacity := 1;

  SetLength(
    fItems, InitialCapacity);
  fCapacity :=
    InitialCapacity;

  fSize := 0;

  SetOnCompareItems(nil);
End;

{--- TGenPriorityQueue.DefaultCompareItems ---}
Function TGenPriorityQueue.DefaultCompareItems(Const A, B: _TItem_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenPriorityQueue.IsEmpty ---}
Function TGenPriorityQueue.IsEmpty: Boolean;
Begin
  Result :=
    (fSize = 0);
End;

{--- TGenPriorityQueue.Pack ---}
Procedure TGenPriorityQueue.Pack;
Begin
  SetLength(
    fItems, fSize);
  fCapacity :=
    fSize;
End;

{--- TGenPriorityQueue.Pop ---}
Procedure TGenPriorityQueue.Pop;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Dec(
    fSize);
  If fSize > 0 Then
    MoveDown(
      0, fItems[fSize]);
End;

{--- TGenPriorityQueue.Push ---}
Procedure TGenPriorityQueue.Push(Const Item: _TItem_);
Begin
  If fSize = fCapacity Then
    Reserve(
      fSize + 1);

  If fSize = 0 Then
    fItems
      [0] := Item
  Else
    MoveUp(
      fSize, Item);

  Inc(
    fSize);
End;

{--- TGenPriorityQueue.ReadTop ---}
Procedure TGenPriorityQueue.ReadTop(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    fItems[0];
End;

{--- TGenPriorityQueue.Reserve ---}
Procedure TGenPriorityQueue.Reserve(MinCapacity: Integer);
Var

  NewCapacity: Integer;
Begin
  If MinCapacity > fCapacity Then
  Begin
    If fCapacity <= 128 Then
      NewCapacity :=
        fCapacity * 2
    Else
      NewCapacity :=
        (fCapacity * 3) Div 2;

    If NewCapacity < MinCapacity Then
      NewCapacity :=
        MinCapacity;

    SetLength(
      fItems, NewCapacity);
    fCapacity :=
      NewCapacity;
  End;
End;

{--- TGenPriorityQueue.MoveDown ---}
Procedure TGenPriorityQueue.MoveDown(Index: Integer; Const Item: _TItem_);
Var

  Half, Child, Right: Integer;
Begin
  Half :=
    fSize Shr 1;

  While
    Index < Half Do
  Begin
    Child :=
      (Index Shl 1) + 1;

    Right :=
      Child + 1;

    If (Right < fSize) And (fOnCompareItems(fItems[Child], fItems[Right]) > 0) Then
      Child :=
        Right;

    If fOnCompareItems(Item, fItems[Child]) <= 0 Then
      Break;

    fItems
      [Index] := fItems[Child];
    Index :=
      Child;
  End;
  fItems
    [Index] := Item;
End;

{--- TGenPriorityQueue.SetOnCompareItems ---}
Procedure TGenPriorityQueue.SetOnCompareItems(AValue: TCompareItems);
Begin
  If AValue = nil Then
    fOnCompareItems := @DefaultCompareItems
  Else
    fOnCompareItems :=
      AValue;
End;

{--- TGenPriorityQueue.MoveUp ---}
Procedure TGenPriorityQueue.MoveUp(Index: Integer; Const Item: _TItem_);
Var

  Parent: Integer;
Begin
  While
    Index > 0 Do
  Begin
    Parent :=
      (Index - 1) Shr 1;

    If fOnCompareItems(Item, fItems[Parent]) >= 0 Then
      Break;

    fItems
      [Index] := fItems[Parent];
    Index :=
      Parent;
  End;
  fItems
    [Index] := Item;
End;

{--- TGenPriorityQueue.Top ---}
Function TGenPriorityQueue.Top: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    fItems[0];
End;

{=================}
{=== TGenQueue ===}
{=================}

{--- TGenQueue.Append ---}
Procedure TGenQueue.Append(Const Item: _TItem_);
Begin
  fData.Append(Item);
End;

{--- TGenQueue.Clear ---}
Procedure TGenQueue.Clear;
Begin
  fData.Clear;
End;

{--- TGenQueue.Create ---}
Constructor TGenQueue.Create;
Begin
  Inherited
  Create;
  fData :=
    _TContainer_.Create;
End;

{--- TGenQueue.Destroy ---}
Destructor TGenQueue.Destroy;
Begin
  fData.Free;
  Inherited
  Destroy;
End;

{--- TGenQueue.Front ---}
Function TGenQueue.Front: _TItem_;
Begin
  fData.
    ReadFirstItem(Result);
End;

{--- TGenQueue.GetSize ---}
Function TGenQueue.GetSize: Integer;
Begin
  Result :=
    fData.Size;
End;

{--- TGenQueue.IsEmpty ---}
Function TGenQueue.IsEmpty: Boolean;
Begin
  Result :=
    fData.Size = 0;
End;

{--- TGenQueue.Pop ---}
Procedure TGenQueue.Pop;
Begin
  fData.DeleteFirst;
End;

{--- TGenQueue.ReadFront ---}
Procedure TGenQueue.ReadFront(out Value: _TItem_);
Begin
  fData.
    ReadFirstItem(Value);
End;

{=================}
{=== TGenStack ===}
{=================}

{--- TGenStack.Clear ---}
Procedure TGenStack.Clear;
Begin
  fData.Clear;
End;

{--- TGenStack.Create ---}
Constructor TGenStack.Create;
Begin
  Inherited
  Create;
  fData :=
    _TContainer_.Create;
End;

{--- TGenStack.Destroy ---}
Destructor TGenStack.Destroy;
Begin
  fData.Free;
  Inherited
  Destroy;
End;

{--- TGenStack.GetSize ---}
Function TGenStack.GetSize: Integer;
Begin
  Result :=
    fData.Size;
End;

{--- TGenStack.IsEmpty ---}
Function TGenStack.IsEmpty: Boolean;
Begin
  Result :=
    (fData.Size = 0);
End;

{--- TGenStack.Pop ---}
Procedure TGenStack.Pop;
Begin
  fData.DeleteLast;
End;

{--- TGenStack.Push ---}
Procedure TGenStack.Push(Const Item: _TItem_);
Begin
  fData.Append(Item);
End;

{--- TGenStack.ReadTop ---}
Procedure TGenStack.ReadTop(out Value: _TItem_);
Begin
  fData.
    ReadLastItem(Value);
End;

{--- TGenStack.Top ---}
Function TGenStack.Top: _TItem_;
Begin
  fData.
    ReadLastItem(Result);
End;

{======================}
{=== THashMapCursor ===}
{======================}

{--- THashMapCursor.Equals ---}
Function THashMapCursor.Equals(Const Cursor: THashMapCursor): Boolean;
Begin
  Result :=
    (fHashMap = Cursor.fHashMap) And (fBucket = Cursor.fBucket) And
    (fEntry = Cursor.fEntry);
End;

{--- THashMapCursor.HasItem ---}
Function THashMapCursor.HasItem: Boolean;
Begin
  Result :=
    (fEntry <> nil);
End;

{--- THashMapCursor.Init ---}
Constructor THashMapCursor.Init(HashMap: TAbstractHashMap; BucketNum: Integer;
  AEntry, APrevious: Pointer);
Begin
  fHashMap :=
    HashMap;
  fBucket :=
    BucketNum;
  fEntry :=
    AEntry;
  fPrevious :=
    APrevious;
End;

{--- THashMapCursor.IsFirst ---}
Function THashMapCursor.IsFirst: Boolean;
Begin
  Result :=
    fHashMap.CursorIsFirst(Self);
End;

{--- THashMapCursor.IsLast ---}
Function THashMapCursor.IsLast: Boolean;
Begin
  Result :=
    fHashMap.CursorIsLast(Self);
End;

{--- THashMapCursor.IsNil ---}
Function THashMapCursor.IsNil: Boolean;
Begin
  Result :=
    (fEntry = nil);
End;

{--- THashMapCursor.MoveNext ---}
Procedure THashMapCursor.MoveNext;
Begin
  fHashMap.
    CursorMoveNext(Self);
End;

{===================}
{=== TGenHashMap ===}
{===================}

{--- TGenHashMap.AppendBuckets ---}
Procedure TGenHashMap.AppendBuckets(Count: Integer);
Begin
  If Count > 0 Then
  Begin
    ReallocMem(
      fBuckets, SizeOf(PEntry) * (fBucketCount + Count));
    NilifyBuckets(
      fBucketCount, Count);
    fBucketCount :=
      fBucketCount + Count;
  End;
End;

{--- TGenHashMap.CollectEntries ---}
Function TGenHashMap.CollectEntries: PEntry;
Var

  I: Integer;
  FirstEntry, LastEntry: PEntry;
Begin
  Result := nil;

  For
    I := 0 To fBucketCount - 1 Do
  Begin
    FirstEntry :=
      fBuckets[I];

    If FirstEntry <> nil Then
    Begin
      LastEntry :=
        FirstEntry;
      While
        LastEntry^.Next <> nil Do
        LastEntry :=
          LastEntry^.Next;

      LastEntry  ^.Next := Result;
      Result :=
        FirstEntry;
    End;
  End;
End;

{--- TGenHashMap.Clear ---}
Procedure TGenHashMap.Clear;
Var

  I: Integer;
Begin
  For
    I := 0 To fBucketCount - 1 Do
  Begin
    If fBuckets[I] <> nil Then
    Begin
      DisposeEntries(
        fBuckets[I]);
      fBuckets
        [I] := nil;
    End;
  End;

  fSize := 0;
  fFirstNonEmptyBucket := -1;
  fLastNonEmptyBucket := -1;
End;

{--- TGenHashMap.Contains ---}
Function TGenHashMap.Contains(Const Key: _TKey_): Boolean;
Begin
  Result :=
    GetEntry(Key) <> nil;
End;

{--- TGenHashMap.Create ---}
Constructor TGenHashMap.Create(InitialCapacity: Integer);
Begin
  Create(
    InitialCapacity, DEFAULT_HASHMAP_LOAD_FACTOR);
End;

{--- TGenHashMap.Create ---}
Constructor TGenHashMap.Create(InitialCapacity: Integer; MaxLoadFact: Real);
Var

  Capacity: Integer;
Begin
  Inherited
  Create;

  If InitialCapacity <= 0 Then
    InitialCapacity :=
      MIN_BUCKET_COUNT;

  If InitialCapacity > MAX_BUCKET_COUNT Then
    InitialCapacity :=
      MAX_BUCKET_COUNT;

  If MaxLoadFact <= 0 Then
    MaxLoadFact :=
      DEFAULT_HASHMAP_LOAD_FACTOR;

  Capacity :=
    MIN_BUCKET_COUNT;
  While
    Capacity < InitialCapacity Do
    Capacity :=
      Capacity * 2;

  fSize := 0;

  fMaxLoadFactor :=
    MaxLoadFact;
  fThreshold :=
    Round(Capacity * MaxLoadFact);

  fMaxBucketCount :=
    MAX_BUCKET_COUNT;

  fBuckets := nil;
  fBucketCount := 0;
  AppendBuckets(
    Capacity);

  fFirstNonEmptyBucket := -1;
  fLastNonEmptyBucket := -1;

  fNilCursor.
    Init(Self, -1, nil, nil);

  SetOnHashKey(nil);
  SetOnItemToString(nil);
  SetOnKeysEqual(nil);
  SetOnKeyToString(nil);
End;

{--- TGenHashMap.Create ---}
Constructor TGenHashMap.Create(MaxLoadFact: Real);
Begin
  Create(
    MIN_BUCKET_COUNT, MaxLoadFact);
End;

{--- TGenHashMap.Delete ---}
Procedure TGenHashMap.Delete(Const Key: _TKey_);
Var

  Bucket: Integer;
  Entry, Previous: PEntry;
Begin
  Bucket :=
    IndexFor(fOnHashKey(Key));
  Entry :=
    FindEntry(Bucket, Key, Previous);

  If Entry = nil Then
    RaiseKeyNotInMap
  Else
    DeleteEntry(
      Bucket, Entry, Previous);
End;

{--- TGenHashMap.DeleteAt ---}
Procedure TGenHashMap.DeleteAt(Const Position: THashMapCursor);
Begin
  If Position.HashMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  DeleteEntry(
    Position.Bucket, Position.Entry, Position.Previous);
End;

{--- TGenHashMap.DeleteEntry ---}
Procedure TGenHashMap.DeleteEntry(Bucket: Integer; Entry, Previous: PEntry);
Var

  Next: PEntry;
Begin
  Next :=
    Entry^.Next;

  If Previous <> nil Then
    Previous  ^.Next := Next;

  If fBuckets[Bucket] = Entry Then
  Begin
    fBuckets
      [Bucket] := Next;

    If Next = nil Then
    Begin
      If Bucket = fFirstNonEmptyBucket Then
        fFirstNonEmptyBucket :=
          NextNonEmptyBucket(Bucket + 1);

      If Bucket = fLastNonEmptyBucket Then
        fLastNonEmptyBucket :=
          PreviousNonEmptyBucket(Bucket - 1);
    End;
  End;

  Dispose(
    Entry);
  Dec(
    fSize);
End;

{--- TGenHashMap.DisposeEntries ---}
Procedure TGenHashMap.DisposeEntries(E: PEntry);
Var

  N: PEntry;
Begin
  While
    E <> nil Do
  Begin
    N :=
      E^.Next;
    Dispose(E);
    E := N;
  End;
End;

{--- TGenHashMap.EnumeratorGet ---}
Function TGenHashMap.EnumeratorGet(Const Pos: THashMapCursor): _TItem_;
Begin
  ReadItemAt(
    Pos, Result);
End;

{--- TGenHashMap.EnumeratorNext ---}
Function TGenHashMap.EnumeratorNext(Var Pos: THashMapCursor): Boolean;
Begin
  If Pos.IsNil Then
    Pos := First
  Else
    Pos.MoveNext;
  Result :=
    Pos.HasItem;
End;

{--- TGenHashMap.Destroy ---}
Destructor TGenHashMap.Destroy;
Begin
  Clear;
  FreeMem(
    fBuckets);
  Inherited
  Destroy;
End;

{--- TGenHashMap.Exclude ---}
Procedure TGenHashMap.Exclude(Const Key: _TKey_);
Var

  Bucket: Integer;
  Entry, Previous: PEntry;
Begin
  Bucket :=
    IndexFor(fOnHashKey(Key));
  Entry :=
    FindEntry(Bucket, Key, Previous);

  If Entry <> nil Then
    DeleteEntry(
      Bucket, Entry, Previous);
End;

{--- TGenHashMap.Find ---}
Function TGenHashMap.Find(Const Key: _TKey_): THashMapCursor;
Var

  Bucket: Integer;
  Entry, Previous: PEntry;
Begin
  Bucket :=
    IndexFor(fOnHashKey(Key));

  Entry :=
    FindEntry(Bucket, Key, Previous);

  Result.
    Init(Self, Bucket, Entry, Previous);
End;

{--- TGenHashMap.FindEntry ---}
Function TGenHashMap.FindEntry(Bucket: Integer; Const Key: _TKey_): PEntry;
Begin
  Result :=
    fBuckets[Bucket];
  While
    Result <> nil Do
  Begin
    If fOnKeysEqual(Result^.Key, Key) Then
      Break;
    Result :=
      Result^.Next;
  End;
End;

{--- TGenHashMap.FindEntry ---}
Function TGenHashMap.FindEntry(Bucket: Integer; Const Key: _TKey_;
  out Previous: PEntry): PEntry;
Begin
  Previous := nil;
  Result :=
    fBuckets[Bucket];
  While
    Result <> nil Do
  Begin
    If fOnKeysEqual(Result^.Key, Key) Then
      Break;
    Previous :=
      Result;
    Result :=
      Result^.Next;
  End;
End;

{--- TGenHashMap.First ---}
Function TGenHashMap.First: THashMapCursor;
Begin
  If fSize > 0 Then
    Result.
      Init(Self, fFirstNonEmptyBucket,
      fBuckets[fFirstNonEmptyBucket], nil)
  Else
    Result :=
      fNilCursor;
End;

{--- TGenHashMap.GetEnumerator ---}
Function TGenHashMap.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenHashMap.GetEntry ---}
Function TGenHashMap.GetEntry(Const Key: _TKey_): PEntry;
Begin
  Result :=
    FindEntry(IndexFor(fOnHashKey(Key)), Key);
End;

{--- TGenHashMap.GetEntryAt ---}
Function TGenHashMap.GetEntryAt(Const Position: THashMapCursor): PEntry;
Begin
  If Position.HashMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  Result :=
    Position.Entry;
End;

{--- TGenHashMap.GetItem ---}
Function TGenHashMap.GetItem(Const Key: _TKey_): _TItem_;
Var

  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);

  If Entry = nil Then
    RaiseKeyNotInMap
  Else
    Result :=
      Entry^.Value;
End;

{--- TGenHashMap.GetItemAt ---}
Function TGenHashMap.GetItemAt(Const Position: THashMapCursor): _TItem_;
Begin
  Result :=
    GetEntryAt(Position)^.Value;
End;

{--- TGenHashMap.GetKeyAt ---}
Function TGenHashMap.GetKeyAt(Const Position: THashMapCursor): _TKey_;
Begin
  Result :=
    GetEntryAt(Position)^.Key;
End;

{--- TGenHashMap.DefaultHashKey ---}
Function TGenHashMap.DefaultHashKey(Const Key: _TKey_): Integer;
Begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenHashMap.GetLoadFactor ---}
Function TGenHashMap.GetLoadFactor: Real;
Begin
  Result :=
    fSize / fBucketCount;
End;

{--- TGenHashMap.Include ---}
Procedure TGenHashMap.Include(Const Key: _TKey_; Const Value: _TItem_);
Var

  Hash, Bucket: Integer;
  Entry, Previous: PEntry;
Begin
  Hash :=
    fOnHashKey(Key);
  Bucket :=
    IndexFor(Hash);

  Entry :=
    FindEntry(Bucket, Key, Previous);
  If Entry <> nil Then
    Entry  ^.Value := Value
  Else
  Begin
    Entry :=
      NewEntry(Key, Value);

    If Previous <> nil Then
      InsertEntry(
        Entry, Previous)
    Else
      InsertEntry(
        Bucket, Entry);
  End;
End;

{--- TGenHashMap.IndexFor ---}
Function TGenHashMap.IndexFor(Hash: Integer): Integer;
Begin
  Result :=
    Longword(Hash) And (fBucketCount - 1);
End;

{--- TGenHashMap.InsertCollectedEntries ---}
Procedure TGenHashMap.InsertCollectedEntries(CollectedEntries: PEntry);
Var

  Entry, NextEntry: PEntry;
  Bucket: Integer;
Begin
  Entry :=
    CollectedEntries;
  While
    Entry <> nil Do
  Begin
    NextEntry :=
      Entry^.Next;

    Bucket :=
      IndexFor(fOnHashKey(Entry^.Key));
    Entry  ^.Next := fBuckets[Bucket];
    fBuckets
      [Bucket] := Entry;

    Entry :=
      NextEntry;
  End;
End;

{--- TGenHashMap.Insert ---}
Procedure TGenHashMap.Insert(Const Key: _TKey_; Const Value: _TItem_);
Var

  Inserted: Boolean;
Begin
  Insert(
    Key, Value, Inserted);
  If Not Inserted Then
    RaiseKeyAlreadyInMap;
End;

{--- TGenHashMap.Insert ---}
Procedure TGenHashMap.Insert(Const Key: _TKey_; Const Value: _TItem_;
  out Inserted: Boolean);
Var

  Hash, Bucket: Integer;
  Entry, Previous: PEntry;
Begin
  Hash :=
    fOnHashKey(Key);
  Bucket :=
    IndexFor(Hash);

  Entry :=
    FindEntry(Bucket, Key, Previous);

  If Entry <> nil Then
    Inserted := False
  Else
  Begin
    Entry :=
      NewEntry(Key, Value);

    If Previous <> nil Then
      InsertEntry(
        Entry, Previous)
    Else
      InsertEntry(
        Bucket, Entry);

    Inserted := True;
  End;
End;

{--- TGenHashMap.InsertEntry ---}
Procedure TGenHashMap.InsertEntry(Bucket: Integer; Entry: PEntry);
Begin
  Entry  ^.Next := fBuckets[Bucket];
  fBuckets
    [Bucket] := Entry;

  If (fFirstNonEmptyBucket = -1) Or (Bucket < fFirstNonEmptyBucket) Then
    fFirstNonEmptyBucket :=
      Bucket;

  If (fLastNonEmptyBucket = -1) Or (Bucket > fLastNonEmptyBucket) Then
    fLastNonEmptyBucket :=
      Bucket;

  Inc(
    fSize);
  If fSize > fThreshold Then
    Resize(
      2 * fBucketCount);
End;

{--- TGenHashMap.InsertEntry ---}
Procedure TGenHashMap.InsertEntry(Entry, Before: PEntry);
Begin
  Before  ^.Next := Entry;
  Entry  ^.Next := nil;

  Inc(
    fSize);
  If fSize > fThreshold Then
    Resize(
      2 * fBucketCount);
End;

{--- TGenHashMap.IsEmpty ---}
Function TGenHashMap.IsEmpty: Boolean;
Begin
  Result :=
    (fSize = 0);
End;

{--- TGenHashMap.DefaultItemToString ---}
Function TGenHashMap.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenHashMap.DefaultKeysEqual ---}
Function TGenHashMap.DefaultKeysEqual(Const A, B: _TKey_): Boolean;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result :=
    False;
End;

{--- TGenHashMap.DefaultKeyToString ---}
Function TGenHashMap.DefaultKeyToString(Const Key: _TKey_): String;
Begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenHashMap.NextNonEmptyBucket ---}
Function TGenHashMap.NextNonEmptyBucket(Bucket: Integer): Integer;
Var

  I: Integer;
Begin
  Result := -1;
  For
    I := Bucket To fBucketCount - 1 Do
    If fBuckets[I] <> nil Then
    Begin
      Result := I;
      Exit;
    End;
End;

{--- TGenHashMap.NewEntry ---}
Function TGenHashMap.NewEntry(Const Key: _TKey_; Const Value: _TItem_): PEntry;
Begin
  New(
    Result);
  Result  ^.Key := Key;
  Result  ^.Value := Value;
End;

{--- TGenHashMap.NilifyBuckets ---}
Procedure TGenHashMap.NilifyBuckets(BucketFrom, Count: Integer);
Var

  I: Integer;
Begin
  For
    I := BucketFrom To BucketFrom + Count - 1 Do
    fBuckets
      [I] := nil;
End;

{--- TGenHashMap.PreviousNonEmptyBucket ---}
Function TGenHashMap.PreviousNonEmptyBucket(Bucket: Integer): Integer;
Var

  I: Integer;
Begin
  Result := -1;
  For
    I := Bucket Downto 0 Do
    If fBuckets[I] <> nil Then
    Begin
      Result := I;
      Break;
    End;
End;

{--- TGenHashMap.ReadItem ---}
Procedure TGenHashMap.ReadItem(Const Key: _TKey_; out Value: _TItem_);
Var

  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);

  If Entry = nil Then
    RaiseKeyNotInMap
  Else
    Value :=
      Entry^.Value;
End;

{--- TGenHashMap.ReadItemAt ---}
Procedure TGenHashMap.ReadItemAt(Const Position: THashMapCursor; out Value: _TItem_);
Begin
  Value :=
    GetEntryAt(Position)^.Value;
End;

{--- TGenHashMap.ReadKeyAt ---}
Procedure TGenHashMap.ReadKeyAt(Const Position: THashMapCursor; out Key: _TKey_);
Begin
  Key :=
    GetEntryAt(Position)^.Key;
End;

{--- TGenHashMap.Replace ---}
Procedure TGenHashMap.Replace(Const Key: _TKey_; Const Value: _TItem_);
Var

  Bucket: Integer;
  Entry: PEntry;
Begin
  Bucket :=
    IndexFor(fOnHashKey(Key));

  Entry :=
    FindEntry(Bucket, Key);

  If Entry = nil Then
    RaiseKeyNotInMap;

  Entry  ^.Value := Value;
End;

{--- TGenHashMap.Resize ---}
Procedure TGenHashMap.Resize(NewCapacity: Integer);
Var

  CollectedEntries: PEntry;
  OldCapacity: Integer;
Begin
  OldCapacity :=
    fBucketCount;

  If OldCapacity = MAX_BUCKET_COUNT Then
  Begin
    fThreshold :=
      High(Integer);
    Exit;
  End;

  { Collect all entries }
  CollectedEntries :=
    CollectEntries;

  If (fFirstNonEmptyBucket >= 0) And (fLastNonEmptyBucket >= 0) Then
    NilifyBuckets(
      fFirstNonEmptyBucket, fLastNonEmptyBucket - fFirstNonEmptyBucket + 1)
  Else
    NilifyBuckets(
      0, fBucketCount);

  { Create necessary buckets }
  AppendBuckets(
    NewCapacity - OldCapacity);
  fThreshold :=
    Round(NewCapacity * fMaxLoadFactor);

  { Re-insert collected entries }
  InsertCollectedEntries(
    CollectedEntries);

  fFirstNonEmptyBucket :=
    NextNonEmptyBucket(0);
  fLastNonEmptyBucket :=
    PreviousNonEmptyBucket(fBucketCount - 1);
End;

{--- TGenHashMap.SetOnHashKey ---}
Procedure TGenHashMap.SetOnHashKey(AValue: THashKey);
Begin
  If AValue = nil Then
    fOnHashKey := @DefaultHashKey
  Else
    fOnHashKey :=
      AValue;
End;

{--- TGenHashMap.SetOnItemToString ---}
Procedure TGenHashMap.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fOnItemToString := @DefaultItemToString
  Else
    fOnItemToString :=
      AValue;
End;

{--- TGenHashMap.SetOnKeysEqual ---}
Procedure TGenHashMap.SetOnKeysEqual(AValue: TKeysEqual);
Begin
  If AValue = nil Then
    fOnKeysEqual := @DefaultKeysEqual
  Else
    fOnKeysEqual :=
      AValue;
End;

{--- TGenHashMap.SetOnKeyToString ---}
Procedure TGenHashMap.SetOnKeyToString(AValue: TKeyToString);
Begin
  If AValue = nil Then
    fOnKeyToString := @DefaultKeyToString
  Else
    fOnKeyToString :=
      AValue;
End;

{--- TGenHashMap.CursorIsFirst ---}
Function TGenHashMap.CursorIsFirst(Const Cursor: THashMapCursor): Boolean;
Var

  Map: TGenHashMap;
Begin
  Map :=
    Cursor.HashMap As TGenHashMap;
  Result :=
    False;
  If Cursor.Bucket = Map.fFirstNonEmptyBucket Then
    Result :=
      Map.fBuckets[Map.fFirstNonEmptyBucket] = Cursor.Entry;
End;

{--- TGenHashMap.CursorIsLast ---}
Function TGenHashMap.CursorIsLast(Const Cursor: THashMapCursor): Boolean;
Var

  Map: TGenHashMap;
  Entry: PEntry;
Begin
  Map :=
    Cursor.HashMap As TGenHashMap;
  Entry :=
    PEntry(Cursor.Entry);
  Result :=
    (Cursor.Bucket = Map.fLastNonEmptyBucket) And (Entry^.Next = nil);
End;

{--- TGenHashMap.CursorMoveNext ---}
Procedure TGenHashMap.CursorMoveNext(Const Cursor: THashMapCursor);
Var

  Map: TGenHashMap;
Begin
  If Cursor.Bucket <> -1 Then
  Begin
    Map :=
      Cursor.HashMap As TGenHashMap;

    Cursor.
      Previous := Cursor.Entry;
    Cursor.Entry := PEntry(Cursor.Entry)^.Next;
    If Cursor.Entry = nil Then
    Begin
      Cursor.Bucket := Map.NextNonEmptyBucket(Cursor.Bucket + 1);
      Cursor.
        Previous := nil;
      If Cursor.Bucket >= 0 Then
        Cursor.Entry := Map.fBuckets[Cursor.Bucket];
    End;
  End;
End;

{--- TGenHashMap.SetItemAt ---}
Procedure TGenHashMap.SetItemAt(Const Position: THashMapCursor; AValue: _TItem_);
Begin
  GetEntryAt(
    Position)^.Value := AValue;
End;

{--- TGenHashMap.ToString ---}
Function TGenHashMap.ToString: String;
Var

  Bucket, LastBucket, I: Integer;
  Entry: PEntry;
Begin
  Result := '{';

  I := 1;
  LastBucket :=
    fBucketCount - 1;
  For
    Bucket := 0 To LastBucket Do
  Begin
    Entry :=
      fBuckets[Bucket];

    While
      Entry <> nil Do
    Begin
      Result :=
        Result + '(' + fOnKeyToString(Entry^.Key) + '=>' +
        fOnItemToString(Entry^.Value) + ')';

      If I < fSize Then
        Result :=
          Result + ', ';

      Inc(I);
      Entry :=
        Entry^.Next;
    End;
  End;

  Result :=
    Result + '}';
End;

{======================}
{=== THashSetCursor ===}
{======================}

{--- THashSetCursor.Equals ---}
Function THashSetCursor.Equals(Const Cursor: THashSetCursor): Boolean;
Begin
  Result :=
    fPos.Equals(Cursor.fPos);
End;

{--- THashSetCursor.HasItem ---}
Function THashSetCursor.HasItem: Boolean;
Begin
  Result :=
    fPos.HasItem;
End;

{--- THashSetCursor.Init ---}
Constructor THashSetCursor.Init(HashSet: TAbstractHashSet; Const APos: THashMapCursor);
Begin
  fHashSet :=
    HashSet;
  fPos := APos;
End;

{--- THashSetCursor.IsFirst ---}
Function THashSetCursor.IsFirst: Boolean;
Begin
  Result :=
    fPos.IsFirst;
End;

{--- THashSetCursor.IsLast ---}
Function THashSetCursor.IsLast: Boolean;
Begin
  Result :=
    fPos.IsLast;
End;

{--- THashSetCursor.IsNil ---}
Function THashSetCursor.IsNil: Boolean;
Begin
  Result :=
    fPos.IsNil;
End;

{--- THashSetCursor.MoveNext ---}
Procedure THashSetCursor.MoveNext;
Begin
  fPos.MoveNext;
End;

{===================}
{=== TGenHashSet ===}
{===================}

{--- TGenHashSet.Clear ---}
Procedure TGenHashSet.Clear;
Begin
  fMap.Clear;
End;

{--- TGenHashSet.Contains ---}
Function TGenHashSet.Contains(Const Item: _TItem_): Boolean;
Begin
  Result :=
    fMap.Contains(Item);
End;

{--- TGenHashSet.Create ---}
Constructor TGenHashSet.Create(InitialCapacity: Integer);
Begin
  Create(
    InitialCapacity, 0.75);
End;

{--- TGenHashSet.Create ---}
Constructor TGenHashSet.Create(InitialCapacity: Integer; LoadFact: Real);
Begin
  fMap :=
    TMap.Create(InitialCapacity, LoadFact);
  fNilCursor.
    Init(Self, fMap.NilCursor);
  SetOnHashItem(nil);
  SetOnItemToString(nil);
  SetOnItemsEqual(nil);
End;

{--- TGenHashSet.Create ---}
Constructor TGenHashSet.Create(LoadFact: Real);
Begin
  Create(
    16, LoadFact);
End;

{--- TGenHashSet.Delete ---}
Procedure TGenHashSet.Delete(Const Item: _TItem_);
Var

  C: THashMapCursor;
Begin
  C :=
    fMap.Find(Item);

  If C.IsNil Then
    RaiseItemNotInSet;

  fMap.DeleteAt(C);
End;

{--- TGenHashSet.DeleteAt ---}
Procedure TGenHashSet.DeleteAt(Const Position: THashSetCursor);
Begin
  fMap.
    DeleteAt(Position.Pos);
End;

{--- TGenHashSet.Destroy ---}
Destructor TGenHashSet.Destroy;
Begin
  fMap.Free;
  Inherited
  Destroy;
End;

{--- TGenHashSet.Difference ---}
Procedure TGenHashSet.Difference(Left, Right: TGenHashSet);
Begin
  If Left <> Self Then
  Begin
    Clear;
    IncludeAll(Left);
  End;

  If Left <> Right Then
    ExcludeAll(Right)
  Else
    Clear;
End;

{--- TGenHashSet.DefaultItemsEqual ---}
Function TGenHashSet.DefaultItemsEqual(Const A, B: _TItem_): Boolean;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := True;
End;

{--- TGenHashSet.DefaultItemToString ---}
Function TGenHashSet.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenHashSet.DefaultHashItem ---}
Function TGenHashSet.DefaultHashItem(Const Item: _TItem_): Integer;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenHashSet.EnumeratorGet ---}
Function TGenHashSet.EnumeratorGet(Const Pos: THashSetCursor): _TItem_;
Begin
  ReadItemAt(
    Pos, Result);
End;

{--- TGenHashSet.EnumeratorNext ---}
Function TGenHashSet.EnumeratorNext(Var Pos: THashSetCursor): Boolean;
Begin
  If Pos.IsNil Then
    Pos := First
  Else
    Pos.MoveNext;
  Result :=
    Pos.HasItem;
End;

{--- TGenHashSet.ExchangeContent ---}
Procedure TGenHashSet.ExchangeContent(ASet: TGenHashSet);
Var

  Tmp: TMap;
Begin
  Tmp := fMap;
  fMap :=
    ASet.fMap;
  ASet.fMap := Tmp;
End;

{--- TGenHashSet.GetItemToString ---}
Function TGenHashSet.GetItemToString: TItemToString;
Begin
  Result :=
    fMap.OnKeyToString;
End;

{--- TGenHashSet.GetOnHashItem ---}
Function TGenHashSet.GetOnHashItem: THashItem;
Begin
  Result :=
    fMap.OnHashKey;
End;

{--- TGenHashSet.GetOnItemsEqual ---}
Function TGenHashSet.GetOnItemsEqual: TItemEquals;
Begin
  Result :=
    fMap.OnKeysEqual;
End;

{--- TGenHashSet.Exclude ---}
Procedure TGenHashSet.Exclude(Const Item: _TItem_);
Begin
  fMap.
    Exclude(Item);
End;

{--- TGenHashSet.ExcludeAll ---}
Procedure TGenHashSet.ExcludeAll(ASet: TGenHashSet);
Var

  C: THashMapCursor;
  I: Integer;
Begin
  If ASet.GetSize > 0 Then
  Begin
    C :=
      ASet.fMap.First;
    For
      I := 1 To ASet.GetSize Do
    Begin
      Exclude(
        ASet.fMap.Keys[C]);
      C.
        MoveNext;
    End;
  End;
End;

{--- TGenHashSet.First ---}
Function TGenHashSet.First: THashSetCursor;
Begin
  Result.
    Init(Self, fMap.First);
End;

{--- TGenHashSet.GetEnumerator ---}
Function TGenHashSet.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenHashSet.GetItemAt ---}
Function TGenHashSet.GetItemAt(Const Position: THashSetCursor): _TItem_;
Begin
  fMap.
    ReadKeyAt(Position.Pos, Result);
End;

{--- TGenHashSet.GetSize ---}
Function TGenHashSet.GetSize: Integer;
Begin
  Result :=
    fMap.Size;
End;

{--- TGenHashSet.SetOnHashItem ---}
Procedure TGenHashSet.SetOnHashItem(AValue: THashItem);
Begin
  If AValue = nil Then
    fMap.
      OnHashKey := @DefaultHashItem
  Else
    fMap.
      OnHashKey := AValue;
End;

{--- TGenHashSet.SetOnItemsEqual ---}
Procedure TGenHashSet.SetOnItemsEqual(AValue: TItemEquals);
Begin
  If AValue = nil Then
    fMap.
      OnKeysEqual := @DefaultItemsEqual
  Else
    fMap.
      OnKeysEqual := AValue;
End;

{--- TGenHashSet.SetOnItemToString ---}
Procedure TGenHashSet.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fMap.
      OnKeyToString := @DefaultItemToString
  Else
    fMap.
      OnKeyToString := AValue;
End;

{--- TGenHashSet.Include ---}
Procedure TGenHashSet.Include(Const Item: _TItem_);
Begin
  fMap.
    Include(Item, 0);
End;

{--- TGenHashSet.IncludeAll ---}
Procedure TGenHashSet.IncludeAll(ASet: TGenHashSet);
Var

  C: THashMapCursor;
  I: Integer;
Begin
  If ASet.GetSize > 0 Then
  Begin
    C :=
      ASet.fMap.First;
    For
      I := 1 To ASet.GetSize Do
    Begin
      Include(
        ASet.fMap.Keys[C]);
      C.
        MoveNext;
    End;
  End;
End;

{--- TGenHashSet.Insert ---}
Procedure TGenHashSet.Insert(Const Item: _TItem_);
Var

  Inserted: Boolean;
Begin
  Insert(
    Item, Inserted);
  If Not Inserted Then
    RaiseItemAlreadyInSet;
End;

{--- TGenHashSet.Insert ---}
Procedure TGenHashSet.Insert(Const Item: _TItem_; out Inserted: Boolean);
Begin
  fMap.
    Insert(Item, 0, Inserted);
End;

{--- TGenHashSet.Intersection ---}
Procedure TGenHashSet.Intersection(Left, Right: TGenHashSet);
Var

  Inter, Tmp: TGenHashSet;
  I: Integer;
  C: THashMapCursor;
  Item: _TItem_;
Begin
  If (Left.GetSize = 0) Or (Right.GetSize = 0) Then
    Clear
  Else
  Begin
    Inter :=
      TGenHashSet.Create;
    Inter.
      OnHashItem := OnHashItem;
    Inter.
      OnItemsEqual := OnItemsEqual;
    Inter.
      OnItemToString := OnItemToString;

    Try
      If Left.GetSize < Right.GetSize Then
      Begin
        Tmp :=
          Left;
        Left :=
          Right;
        Right := Tmp;
      End;

      C :=
        Left.fMap.First;
      For
        I := 1 To Left.GetSize Do
      Begin
        Item :=
          Left.fMap.Keys[C];
        If Right.fMap.Contains(Item) Then
          Inter.
            Include(Item);
        C.
          MoveNext;
      End;

      ExchangeContent(
        Inter);
    Finally
      Inter.Free;
    End;
  End;
End;

{--- TGenHashSet.IsEmpty ---}
Function TGenHashSet.IsEmpty: Boolean;
Begin
  Result :=
    (fMap.Size = 0);
End;

{--- TGenHashSet.IsSubset ---}
Function TGenHashSet.IsSubset(OfSet: TGenHashSet): Boolean;
Var

  I: Integer;
  C: THashMapCursor;
Begin
  If GetSize > 0 Then
  Begin
    C := fMap.First;
    For
      I := 1 To GetSize Do
    Begin
      If Not OfSet.fMap.Contains(fMap.Keys[C]) Then
      Begin
        Result :=
          False;
        Exit;
      End;
      C.
        MoveNext;
    End;
  End;
  Result := True;
End;

{--- TGenHashSet.Overlaps ---}
Function TGenHashSet.Overlaps(ASet: TGenHashSet): Boolean;
Var

  I: Integer;
  C: THashMapCursor;
Begin
  Result := False;
  If GetSize > 0 Then
  Begin
    C := fMap.First;
    For
      I := 1 To GetSize Do
    Begin
      If ASet.fMap.Contains(fMap.Keys[C]) Then
      Begin
        Result := True;
        Break;
      End;
      C.
        MoveNext;
    End;
  End;
End;

{--- TGenHashSet.ReadItemAt ---}
Procedure TGenHashSet.ReadItemAt(Const Position: THashSetCursor; out Value: _TItem_);
Begin
  fMap.
    ReadKeyAt(Position.Pos, Value);
End;

{--- TGenHashSet.SymmetricDifference ---}
Procedure TGenHashSet.SymmetricDifference(Left, Right: TGenHashSet);
Var

  Inter: TGenHashSet;
Begin
  Inter :=
    TGenHashSet.Create;
  Inter.
    OnHashItem := OnHashItem;
  Inter.
    OnItemsEqual := OnItemsEqual;
  Inter.
    OnItemToString := OnItemToString;

  Try
    Inter.
      Intersection(Left, Right);

    Union(
      Left, Right);
    Difference(
      Self, Inter);
  Finally
    Inter.Free;
  End;
End;

{--- TGenHashSet.ToString ---}
Function TGenHashSet.ToString: String;
Var
  C: THashMapCursor;
Begin
  Result := '{';
  If GetSize > 0 Then
  Begin
    C := fMap.First;
    While
      C.HasItem Do
    Begin
      Result :=
        Result + fMap.OnKeyToString(fMap.Keys[C]);
      If Not C.IsLast Then
        Result :=
          Result + '; ';
      C.MoveNext;
    End;
  End;
  Result :=
    Result + '}';
End;

{--- TGenHashSet.Union ---}
Procedure TGenHashSet.Union(Left, Right: TGenHashSet);
Begin
  If Left <> Self Then
  Begin
    Clear;
    IncludeAll(Left);
  End;

  If Left <> Right Then
    IncludeAll(
      Right);
End;

{======================}
{=== TTreeMapCursor ===}
{======================}

{--- TTreeMapCursor.Equals ---}
Function TTreeMapCursor.Equals(Const Cursor: TTreeMapCursor): Boolean;
Begin
  Result :=
    (fTreeMap = Cursor.fTreeMap) And (fEntry = Cursor.fEntry);
End;

{--- TTreeMapCursor.HasItem ---}
Function TTreeMapCursor.HasItem: Boolean;
Begin
  Result :=
    (fEntry <> nil);
End;

{--- TTreeMapCursor.Init ---}
Constructor TTreeMapCursor.Init(Map: TAbstractTreeMap; AnEntry: Pointer);
Begin
  fTreeMap := Map;
  fEntry := AnEntry;
End;

{--- TTreeMapCursor.IsFirst ---}
Function TTreeMapCursor.IsFirst: Boolean;
Begin
  Result :=
    fTreeMap.CursorIsFirst(Self);
End;

{--- TTreeMapCursor.IsLast ---}
Function TTreeMapCursor.IsLast: Boolean;
Begin
  Result :=
    fTreeMap.CursorIsLast(Self);
End;

{--- TTreeMapCursor.IsNil ---}
Function TTreeMapCursor.IsNil: Boolean;
Begin
  Result :=
    (fEntry = nil);
End;

{--- TTreeMapCursor.MoveNext ---}
Procedure TTreeMapCursor.MoveNext;
Begin
  fTreeMap.
    CursorMoveNext(Self);
End;

{--- TTreeMapCursor.MovePrevious ---}
Procedure TTreeMapCursor.MovePrevious;
Begin
  fTreeMap.
    CursorMovePrev(Self);
End;

{===================}
{=== TGenTreeMap ===}
{===================}

{--- TGenTreeMap.Ceiling ---}
Function TGenTreeMap.Ceiling(Const Key: _TKey_): TTreeMapCursor;
Begin
  Result.
    Init(Self, GetCeilingEntry(Key));
End;

{--- TGenTreeMap.Clear ---}
Procedure TGenTreeMap.Clear;
Begin
  DeleteTree(fRoot);
  fRoot := nil;
End;

{--- TGenTreeMap.ColorOf ---}
Function TGenTreeMap.ColorOf(E: PEntry): TColor;
Begin
  If E = nil Then
    Result := cBlack
  Else
    Result := E^.Color;
End;

{--- TGenTreeMap.Contains ---}
Function TGenTreeMap.Contains(Const Key: _TKey_): Boolean;
Begin
  Result :=
    GetEntry(Key) <> nil;
End;

{--- TGenTreeMap.Create ---}
Constructor TGenTreeMap.Create;
Begin
  Inherited
  Create;
  fSize := 0;
  fRoot := nil;
  fNilCursor.
    Init(Self, nil);
  SetOnCompareKeys(nil);
  SetOnItemToString(nil);
  SetOnKeyToString(nil);
End;

{--- TGenTreeMap.DefaultCompareKeys ---}
Function TGenTreeMap.DefaultCompareKeys(Const A, B: _TKey_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenTreeMap.DefaultItemToString ---}
Function TGenTreeMap.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenTreeMap.DefaultKeyToString ---}
Function TGenTreeMap.DefaultKeyToString(Const Key: _TKey_): String;
Begin
  Unused(@Key);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenTreeMap.Delete ---}
Procedure TGenTreeMap.Delete(Const Key: _TKey_);
Var
  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);
  If Entry = nil Then
    RaiseKeyNotInMap;

  DeleteEntry(Entry);
End;

{--- TGenTreeMap.DeleteAt ---}
Procedure TGenTreeMap.DeleteAt(Const Position: TTreeMapCursor);
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  DeleteEntry(
    Position.Entry);
End;

{--- TGenTreeMap.DeleteEntry ---}
Procedure TGenTreeMap.DeleteEntry(E: PEntry);
Var
  S, Replacement: PEntry;
Begin
  Dec(
    fSize);

  If (E^.Left <> nil) And (E^.Right <> nil) Then
  Begin
    S :=
      Successor(E);
    E^.Key := S^.Key;
    E^.Value := S^.Value;
    E := S;
  End;

  If E^.Left <> nil Then
    Replacement :=
      E^.Left
  Else
    Replacement :=
      E^.Right;

  If Replacement <> nil Then
  Begin
    Replacement  ^.Parent := E^.Parent;

    If E^.Parent = nil Then
      fRoot :=
        Replacement
    Else
      If E = E^.Parent^.Left Then
        E^.Parent^.Left := Replacement
      Else
        E^.Parent^.Right := Replacement;

    E  ^.Left := nil;
    E^.Right := nil;
    E^.Parent := nil;

    If E^.Color = cBlack Then
      RepairAfterDelete(
        Replacement);
  End
  Else
    If E^.Parent = nil Then
      fRoot := nil
    Else
    Begin
      If E^.Color = cBlack Then
        RepairAfterDelete(E);

      If E^.Parent <> nil Then
      Begin
        If E = E^.Parent^.Left Then
          E^.Parent^.Left := nil
        Else
          If E = E^.Parent^.Right Then
            E^.Parent^.Right := nil;

        E^.Parent := nil;
      End;
    End;
  Dispose(E);
End;

{--- TGenTreeMap.DeleteFirst ---}
Procedure TGenTreeMap.DeleteFirst;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  DeleteEntry(
    GetFirstEntry);
End;

{--- TGenTreeMap.DeleteLast ---}
Procedure TGenTreeMap.DeleteLast;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  DeleteEntry(
    GetLastEntry);
End;

{--- TGenTreeMap.DeleteTree ---}
Procedure TGenTreeMap.DeleteTree(E: PEntry);
Var
  R, L: PEntry;
Begin
  While
    True Do
  Begin
    If E = nil Then
      Exit;

    R := E^.Right;
    L := E^.Left;

    Dispose(E);
    Dec(
      fSize);

    DeleteTree(L);

    E := R;
  End;
End;

{--- TGenTreeMap.EnumeratorGet ---}
Function TGenTreeMap.EnumeratorGet(Const Pos: TTreeMapCursor): _TItem_;
Begin
  ReadItemAt(
    Pos, Result);
End;

{--- TGenTreeMap.EnumeratorNext ---}
Function TGenTreeMap.EnumeratorNext(Var Pos: TTreeMapCursor): Boolean;
Begin
  If Pos.IsNil Then
    Pos := First
  Else
    Pos.MoveNext;
  Result :=
    Pos.HasItem;
End;

{--- TGenTreeMap.Destroy ---}
Destructor TGenTreeMap.Destroy;
Begin
  Clear;
  Inherited
  Destroy;
End;

{--- TGenTreeMap.Exclude ---}
Procedure TGenTreeMap.Exclude(Const Key: _TKey_);
Var
  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);
  If Entry <> nil Then
    DeleteEntry(
      Entry);
End;

{--- TGenTreeMap.Find ---}
Function TGenTreeMap.Find(Const Key: _TKey_): TTreeMapCursor;
Begin
  Result.
    Init(Self, GetEntry(Key));
End;

{--- TGenTreeMap.First ---}
Function TGenTreeMap.First: TTreeMapCursor;
Begin
  Result.
    Init(Self, GetFirstEntry);
End;

{--- TGenTreeMap.FirstItem ---}
Function TGenTreeMap.FirstItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    GetFirstEntry^.Value;
End;

{--- TGenTreeMap.FirstKey ---}
Function TGenTreeMap.FirstKey: _TKey_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    GetFirstEntry^.Key;
End;

{--- TGenTreeMap.RepairAfterDelete ---}
Procedure TGenTreeMap.RepairAfterDelete(E: PEntry);
Var
  Sib: PEntry;
Begin
  While
    (E <> fRoot) And (ColorOf(E) = cBlack) Do
  Begin
    If E = LeftOf(ParentOf(E)) Then
    Begin
      Sib :=
        RightOf(ParentOf(E));

      If ColorOf(Sib) = cRed Then
      Begin
        SetColor(
          Sib, cBlack);
        SetColor(
          ParentOf(E), cRed);
        RotateLeft(
          ParentOf(E));
        Sib :=
          RightOf(ParentOf(E));
      End;

      If (ColorOf(LeftOf(Sib)) = cBlack) And (ColorOf(RightOf(Sib)) = cBlack) Then
      Begin
        SetColor(
          Sib, cRed);
        E :=
          ParentOf(E);
      End
      Else
      Begin
        If ColorOf(RightOf(Sib)) = cBlack Then
        Begin
          SetColor(
            LeftOf(Sib), cBlack);
          SetColor(
            Sib, cRed);
          RotateRight(Sib);
          Sib :=
            RightOf(ParentOf(E));
        End;

        SetColor(
          Sib, ColorOf(ParentOf(E)));
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          RightOf(Sib), cBlack);
        RotateLeft(
          ParentOf(E));
        E :=
          fRoot;
      End;
    End
    Else
    Begin
      Sib :=
        LeftOf(ParentOf(E));

      If ColorOf(Sib) = cRed Then
      Begin
        SetColor(
          Sib, cBlack);
        SetColor(
          ParentOf(E), cRed);
        RotateRight(
          ParentOf(E));
        Sib :=
          LeftOf(ParentOf(E));
      End;

      If (ColorOf(RightOf(Sib)) = cBlack) And (ColorOf(LeftOf(Sib)) = cBlack) Then
      Begin
        SetColor(
          Sib, cRed);
        E :=
          ParentOf(E);
      End
      Else
      Begin

        If ColorOf(LeftOf(Sib)) = cBlack Then
        Begin
          SetColor(
            RightOf(Sib), cBlack);
          SetColor(
            Sib, cRed);
          RotateLeft(Sib);
          Sib :=
            LeftOf(ParentOf(E));
        End;

        SetColor(
          Sib, ColorOf(ParentOf(E)));
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          LeftOf(Sib), cBlack);
        RotateRight(
          ParentOf(E));
        E :=
          fRoot;
      End;
    End;
  End;

  SetColor(
    E, cBlack);
End;

{--- TGenTreeMap.RepairAfterInsert ---}
Procedure TGenTreeMap.RepairAfterInsert(E: PEntry);
Var
  Y: PEntry;
Begin
  E^.Color := cRed;

  While
    (E <> nil) And (E <> fRoot) And (E^.Parent^.Color = cRed) Do
  Begin
    If ParentOf(E) = LeftOf(ParentOf(ParentOf(E))) Then
    Begin
      Y :=
        RightOf(ParentOf(ParentOf(E)));
      If ColorOf(Y) = cRed Then
      Begin
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          Y, cBlack);
        SetColor(
          ParentOf(ParentOf(E)), cRed);
        E :=
          ParentOf(ParentOf(E));
      End
      Else
      Begin
        If E = RightOf(ParentOf(E)) Then
        Begin
          E :=
            ParentOf(E);
          RotateLeft(E);
        End;
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          ParentOf(ParentOf(E)), cRed);
        RotateRight(
          ParentOf(ParentOf(E)));
      End;
    End
    Else
    Begin
      Y :=
        LeftOf(ParentOf(ParentOf(E)));
      If ColorOf(Y) = cRed Then
      Begin
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          Y, cBlack);
        SetColor(
          ParentOf(ParentOf(E)), cRed);
        E :=
          ParentOf(ParentOf(E));
      End
      Else
      Begin
        If E = LeftOf(ParentOf(E)) Then
        Begin
          E :=
            ParentOf(E);
          RotateRight(E);
        End;
        SetColor(
          ParentOf(E), cBlack);
        SetColor(
          ParentOf(ParentOf(E)), cRed);
        RotateLeft(
          ParentOf(ParentOf(E)));
      End;
    End;
  End;

  fRoot  ^.Color := cBlack;
End;

{--- TGenTreeMap.Floor ---}
Function TGenTreeMap.Floor(Const Key: _TKey_): TTreeMapCursor;
Begin
  Result.
    Init(Self, GetFloorEntry(Key));
End;

{--- TGenTreeMap.GetEnumerator ---}
Function TGenTreeMap.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenTreeMap.GetCeilingEntry ---}
Function TGenTreeMap.GetCeilingEntry(Const Key: _TKey_): PEntry;
Var
  Cmp: Integer;
  Ch, Parent: PEntry;
Begin
  Result := fRoot;
  While
    Result <> nil Do
  Begin
    Cmp :=
      fOnCompareKeys(Key, Result^.Key);
    If Cmp < 0 Then
    Begin
      If Result^.Left <> nil Then
        Result :=
          Result^.Left
      Else
        Exit;
    End
    Else
      If Cmp > 0 Then
      Begin
        If Result^.Right <> nil Then
          Result :=
            Result^.Right
        Else
        Begin
          Parent :=
            Result^.Parent;
          Ch :=
            Result;
          While
            (Parent <> nil) And (Ch = Parent^.Right) Do
          Begin
            Ch :=
              Parent;
            Parent :=
              Parent^.Parent;
          End;
          Result :=
            Parent;
          Exit;
        End;
      End
      Else
        Exit;
  End;
  Result := nil;
End;

{--- TGenTreeMap.GetEntry ---}
Function TGenTreeMap.GetEntry(Const Key: _TKey_): PEntry;
Var
  Entry: PEntry;
  Cmp: Integer;
Begin
  Entry := fRoot;
  While
    Entry <> nil Do
  Begin
    Cmp :=
      fOnCompareKeys(Key, Entry^.Key);

    If Cmp < 0 Then
      Entry :=
        Entry^.Left
    Else
      If Cmp > 0 Then
        Entry :=
          Entry^.Right
      Else
      Begin
        Result :=
          Entry;
        Exit;
      End;
  End;
  Result := nil;
End;

{--- TGenTreeMap.GetFirstEntry ---}
Function TGenTreeMap.GetFirstEntry: PEntry;
Begin
  Result := fRoot;
  If Result <> nil Then
    While
      Result^.Left <> nil Do
      Result :=
        Result^.Left;
End;

{--- TGenTreeMap.GetFloorEntry ---}
Function TGenTreeMap.GetFloorEntry(Const Key: _TKey_): PEntry;
Var
  Cmp: Integer;
  Ch, Parent: PEntry;
Begin
  Result := fRoot;
  While
    Result <> nil Do
  Begin
    Cmp :=
      fOnCompareKeys(Key, Result^.Key);
    If Cmp > 0 Then
    Begin
      If Result^.Right <> nil Then
        Result :=
          Result^.Right
      Else
        Exit;
    End
    Else
      If Cmp < 0 Then
      Begin
        If Result^.Left <> nil Then
          Result :=
            Result^.Left
        Else
        Begin
          Parent :=
            Result^.Parent;
          Ch :=
            Result;
          While
            (Parent <> nil) And (Ch = Parent^.Left) Do
          Begin
            Ch :=
              Parent;
            Parent :=
              Parent^.Parent;
          End;
          Result :=
            Parent;
          Exit;
        End;
      End
      Else
        Exit;
  End;
  Result := nil;
End;

{--- TGenTreeMap.GetItem ---}
Function TGenTreeMap.GetItem(Const Key: _TKey_): _TItem_;
Var
  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);
  If Entry = nil Then
    RaiseKeyNotInMap;

  Result :=
    Entry^.Value;
End;

{--- TGenTreeMap.GetItemAt ---}
Function TGenTreeMap.GetItemAt(Const Position: TTreeMapCursor): _TItem_;
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  Result :=
    PEntry(Position.Entry)^.Value;
End;

{--- TGenTreeMap.GetKeyAt ---}
Function TGenTreeMap.GetKeyAt(Const Position: TTreeMapCursor): _TKey_;
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  Result :=
    PEntry(Position.Entry)^.Key;
End;

{--- TGenTreeMap.GetLastEntry ---}
Function TGenTreeMap.GetLastEntry: PEntry;
Begin
  Result := fRoot;
  If Result <> nil Then
    While
      Result^.Right <> nil Do
      Result :=
        Result^.Right;
End;

{--- TGenTreeMap.Include ---}
Procedure TGenTreeMap.Include(Const Key: _TKey_; Const Value: _TItem_);
Var
  T, Parent, N: PEntry;
  Cmp: Integer;
Begin
  If fRoot = nil Then
  Begin
    fRoot :=
      NewEntry(nil, Key, Value);
    fSize := 1;
  End
  Else
  Begin
    T := fRoot;
    Repeat
      Parent := T;
      Cmp :=
        fOnCompareKeys(Key, T^.Key);
      If Cmp < 0 Then
        T := T^.Left
      Else
        If Cmp > 0 Then
          T :=
            T^.Right
        Else
        Begin
          T^.Value := Value;
          Exit;
        End;
    Until
      T = nil;

    N :=
      NewEntry(Parent, Key, Value);
    If Cmp < 0 Then
      Parent  ^.Left := N
    Else
      Parent  ^.Right := N;
    RepairAfterInsert(N);
    Inc(
      fSize);
  End;
End;

{--- TGenTreeMap.Insert ---}
Procedure TGenTreeMap.Insert(Const Key: _TKey_; Const Value: _TItem_);
Var
  Inserted: Boolean;
Begin
  Insert(
    Key, Value, Inserted);
  If Not Inserted Then
    RaiseKeyAlreadyInMap;
End;

{--- TGenTreeMap.Insert ---}
Procedure TGenTreeMap.Insert(Const Key: _TKey_; Const Value: _TItem_;
  out Inserted: Boolean);
Var
  T, Parent, N: PEntry;
  Cmp: Integer;
Begin
  Inserted := False;
  If fRoot = nil Then
  Begin
    fRoot :=
      NewEntry(nil, Key, Value);
    fSize := 1;
    Inserted := True;
  End
  Else
  Begin
    T := fRoot;
    Repeat
      Parent := T;
      Cmp :=
        fOnCompareKeys(Key, T^.Key);
      If Cmp < 0 Then
        T := T^.Left
      Else
        If Cmp > 0 Then
          T :=
            T^.Right
        Else
          Exit;
    Until
      T = nil;

    N :=
      NewEntry(Parent, Key, Value);
    If Cmp < 0 Then
      Parent  ^.Left := N
    Else
      Parent  ^.Right := N;
    RepairAfterInsert(N);
    Inc(
      fSize);
    Inserted := True;
  End;
End;

{--- TGenTreeMap.IsEmpty ---}
Function TGenTreeMap.IsEmpty: Boolean;
Begin
  Result :=
    (fSize = 0);
End;

{--- TGenTreeMap.Last ---}
Function TGenTreeMap.Last: TTreeMapCursor;
Begin
  Result.
    Init(Self, GetLastEntry);
End;

{--- TGenTreeMap.LastItem ---}
Function TGenTreeMap.LastItem: _TItem_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    GetLastEntry^.Value;
End;

{--- TGenTreeMap.LastKey ---}
Function TGenTreeMap.LastKey: _TKey_;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Result :=
    GetLastEntry^.Key;
End;

{--- TGenTreeMap.LeftOf ---}
Function TGenTreeMap.LeftOf(E: PEntry): PEntry;
Begin
  If E = nil Then
    Result := nil
  Else
    Result := E^.Left;
End;

{--- TGenTreeMap.NewEntry ---}
Function TGenTreeMap.NewEntry(AParent: PEntry; Const AKey: _TKey_;
  Const AValue: _TItem_): PEntry;
Begin
  New(
    Result);
  Result  ^.Parent := AParent;
  Result  ^.Key := AKey;
  Result  ^.Value := AValue;
  Result  ^.Left := nil;
  Result  ^.Right := nil;
End;

{--- TGenTreeMap.ParentOf ---}
Function TGenTreeMap.ParentOf(E: PEntry): PEntry;
Begin
  If E = nil Then
    Result := nil
  Else
    Result := E^.Parent;
End;

{--- TGenTreeMap.Predecessor ---}
Function TGenTreeMap.Predecessor(E: PEntry): PEntry;
Var
  Ch: PEntry;
Begin
  If E = nil Then
    Result := nil
  Else
    If E^.Left <> nil Then
    Begin
      Result :=
        E^.Left;
      While
        Result^.Right <> nil Do
        Result :=
          Result^.Right;
    End
    Else
    Begin
      Result :=
        E^.Parent;
      Ch := E;
      While
        (Result <> nil) And (Ch = Result^.Left) Do
      Begin
        Ch := Result;
        Result :=
          Result^.Parent;
      End;
    End;
End;

{--- TGenTreeMap.ReadFirstItem ---}
Procedure TGenTreeMap.ReadFirstItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    GetFirstEntry^.Value;
End;

{--- TGenTreeMap.ReadFirstKey ---}
Procedure TGenTreeMap.ReadFirstKey(out Key: _TKey_); Inline;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Key :=
    GetFirstEntry^.Key;
End;

{--- TGenTreeMap.ReadItem ---}
Procedure TGenTreeMap.ReadItem(Const Key: _TKey_; out Value: _TItem_);
Var
  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);
  If Entry = nil Then
    RaiseKeyNotInMap;

  Value :=
    Entry^.Value;
End;

{--- TGenTreeMap.ReadItemAt ---}
Procedure TGenTreeMap.ReadItemAt(Const Position: TTreeMapCursor; out Value: _TItem_);
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  Value :=
    PEntry(Position.Entry)^.Value;
End;

{--- TGenTreeMap.ReadKeyAt ---}
Procedure TGenTreeMap.ReadKeyAt(Const Position: TTreeMapCursor; out Key: _TKey_);
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  Key :=
    PEntry(Position.Entry)^.Key;
End;

{--- TGenTreeMap.ReadLastItem ---}
Procedure TGenTreeMap.ReadLastItem(out Value: _TItem_);
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Value :=
    GetLastEntry^.Value;
End;

{--- TGenTreeMap.ReadLastKey ---}
Procedure TGenTreeMap.ReadLastKey(out Key: _TKey_); Inline;
Begin
  If fSize = 0 Then
    RaiseContainerEmpty;

  Key :=
    GetLastEntry^.Key;
End;

{--- TGenTreeMap.Replace ---}
Procedure TGenTreeMap.Replace(Const Key: _TKey_; Const Value: _TItem_);
Var
  Entry: PEntry;
Begin
  Entry :=
    GetEntry(Key);
  If Entry = nil Then
    RaiseKeyNotInMap;

  Entry  ^.Value := Value;
End;

{--- TGenTreeMap.RightOf ---}
Function TGenTreeMap.RightOf(E: PEntry): PEntry;
Begin
  If E = nil Then
    Result := nil
  Else
    Result := E^.Right;
End;

{--- TGenTreeMap.RotateLeft ---}
Procedure TGenTreeMap.RotateLeft(E: PEntry);
Var
  R: PEntry;
Begin
  If E <> nil Then
  Begin
    R := E^.Right;

    E^.Right := R^.Left;

    If R^.Left <> nil Then
      R^.Left^.Parent := E;

    R^.Parent := E^.Parent;
    If E^.Parent = nil Then
      fRoot := R
    Else
      If E^.Parent^.Left = E Then
        E^.Parent^.Left := R
      Else
        E^.Parent^.Right := R;
    R  ^.Left := E;
    E  ^.Parent := R;
  End;
End;

{--- TGenTreeMap.RotateRight ---}
Procedure TGenTreeMap.RotateRight(E: PEntry);
Var
  L: PEntry;
Begin
  If E <> nil Then
  Begin
    L := E^.Left;
    E^.Left := L^.Right;
    If L^.Right <> nil Then
      L^.Right^.Parent := E;
    L^.Parent := E^.Parent;
    If E^.Parent = nil Then
      fRoot := L
    Else
      If E^.Parent^.Right = E Then
        E^.Parent^.Right := L
      Else
        E^.Parent^.Left := L;
    L  ^.Right := E;
    E  ^.Parent := L;
  End;
End;

{--- TGenTreeMap.SetColor ---}
Procedure TGenTreeMap.SetColor(E: PEntry; Color: TColor);
Begin
  If E <> nil Then
    E^.Color := Color;
End;

{--- TGenTreeMap.SetOnCompareKeys ---}
Procedure TGenTreeMap.SetOnCompareKeys(AValue: TCompareKeys);
Begin
  If AValue = nil Then
    fOnCompareKeys := @DefaultCompareKeys
  Else
    fOnCompareKeys :=
      AValue;
End;

{--- TGenTreeMap.SetOnItemToString ---}
Procedure TGenTreeMap.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fOnItemToString := @DefaultItemToString
  Else
    fOnItemToString :=
      AValue;
End;

{--- TGenTreeMap.SetOnKeyToString ---}
Procedure TGenTreeMap.SetOnKeyToString(AValue: TKeyToString);
Begin
  If AValue = nil Then
    fOnKeyToString := @DefaultKeyToString
  Else
    fOnKeyToString :=
      AValue;
End;

{--- TGenTreeMap.SetItemAt ---}
Procedure TGenTreeMap.SetItemAt(Const Position: TTreeMapCursor; Value: _TItem_);
Begin
  If Position.TreeMap <> Self Then
    RaiseCursorDenotesWrongContainer;

  If Position.IsNil Then
    RaiseCursorIsNil;

  PEntry(
    Position.Entry)^.Value := Value;
End;

{--- TGenTreeMap.Successor ---}
Function TGenTreeMap.Successor(E: PEntry): PEntry;
Var
  P, Ch: PEntry;
Begin
  If E = nil Then
    Result := nil
  Else
    If E^.Right <> nil Then
    Begin
      P :=
        E^.Right;
      While
        P^.Left <> nil Do
        P :=
          P^.Left;
      Result := P;
    End
    Else
    Begin
      P := E^.Parent;
      Ch := E;
      While
        (P <> nil) And (Ch = P^.Right) Do
      Begin
        Ch := P;
        P := P^.Parent;
      End;
      Result := P;
    End;
End;

{--- TGenTreeMap.CursorIsFirst ---}
Function TGenTreeMap.CursorIsFirst(Const Cursor: TTreeMapCursor): Boolean;
Begin
  Result :=
    (Cursor.Entry <> nil) And (Cursor.Entry = (Cursor.TreeMap As
    TGenTreeMap).GetFirstEntry);
End;

{--- TGenTreeMap.CursorIsLast ---}
Function TGenTreeMap.CursorIsLast(Const Cursor: TTreeMapCursor): Boolean;
Begin
  Result :=
    (Cursor.Entry <> nil) And (Cursor.Entry = (Cursor.TreeMap As
    TGenTreeMap).GetLastEntry);
End;

{--- TGenTreeMap.CursorMoveNext ---}
Procedure TGenTreeMap.CursorMoveNext(Const Cursor: TTreeMapCursor);
Begin
  If Cursor.Entry <> nil Then
    Cursor.
      Entry := (Cursor.TreeMap As TGenTreeMap).Successor(Cursor.Entry);
End;

{--- TGenTreeMap.CursorMovePrev ---}
Procedure TGenTreeMap.CursorMovePrev(Const Cursor: TTreeMapCursor);
Begin
  If Cursor.Entry <> nil Then
    Cursor.
      Entry := (Cursor.TreeMap As TGenTreeMap).Predecessor(Cursor.Entry);
End;

{--- TGenTreeMap.ToString ---}
Function TGenTreeMap.ToString: String;
Var
  Entry, LastEntry: PEntry;
Begin
  Result := '{';

  LastEntry :=
    GetLastEntry;

  Entry :=
    GetFirstEntry;
  While
    Entry <> nil Do
  Begin
    Result :=
      Result + '(' + fOnKeyToString(Entry^.Key) + '=>' + fOnItemToString(
      Entry^.Value) + ')';

    If Entry <> LastEntry Then
      Result :=
        Result + ', ';

    Entry :=
      Successor(Entry);
  End;

  Result :=
    Result + '}';
End;

{======================}
{=== TTreeSetCursor ===}
{======================}

{--- TTreeSetCursor.Equals ---}
Function TTreeSetCursor.Equals(Const Cursor: TTreeSetCursor): Boolean;
Begin
  Result :=
    fPos.Equals(Cursor.fPos);
End;

{--- TTreeSetCursor.HasItem ---}
Function TTreeSetCursor.HasItem: Boolean;
Begin
  Result :=
    fPos.HasItem;
End;

{--- TTreeSetCursor.Init ---}
Constructor TTreeSetCursor.Init(TreeSet: TAbstractTreeSet; Const APos: TTreeMapCursor);
Begin
  fTreeSet := TreeSet;
  fPos := APos;
End;

{--- TTreeSetCursor.IsFirst ---}
Function TTreeSetCursor.IsFirst: Boolean;
Begin
  Result :=
    fPos.IsFirst;
End;

{--- TTreeSetCursor.IsLast ---}
Function TTreeSetCursor.IsLast: Boolean;
Begin
  Result :=
    fPos.IsLast;
End;

{--- TTreeSetCursor.IsNil ---}
Function TTreeSetCursor.IsNil: Boolean;
Begin
  Result := fPos.IsNil;
End;

{--- TTreeSetCursor.MoveNext ---}
Procedure TTreeSetCursor.MoveNext;
Begin
  fPos.MoveNext;
End;

{--- TTreeSetCursor.MovePrevious ---}
Procedure TTreeSetCursor.MovePrevious;
Begin
  fPos.MovePrevious;
End;

{===================}
{=== TGenTreeSet ===}
{===================}

{--- TGenTreeSet.Ceiling ---}
Function TGenTreeSet.Ceiling(Const Item: _TItem_): TTreeSetCursor;
Begin
  Result.
    Init(Self, fMap.Ceiling(Item));
End;

{--- TGenTreeSet.Clear ---}
Procedure TGenTreeSet.Clear;
Begin
  fMap.Clear;
End;

{--- TGenTreeSet.Contains ---}
Function TGenTreeSet.Contains(Const Item: _TItem_): Boolean;
Begin
  Result :=
    fMap.Contains(Item);
End;

{--- TGenTreeSet.Create ---}
Constructor TGenTreeSet.Create;
Begin
  fMap :=
    TMap.Create;
  fNilCursor.
    Init(Self, fMap.NilCursor);
  SetOnCompareItems(nil);
  SetOnItemToString(nil);
End;

{--- TGenTreeSet.DefaultCompareItems ---}
Function TGenTreeSet.DefaultCompareItems(Const A, B: _TItem_): Integer;
Begin
  Unused(@A);
  Unused(@B);
  RaiseMethodNotRedefined;
  Result := 0;
End;

{--- TGenTreeSet.DefaultItemToString ---}
Function TGenTreeSet.DefaultItemToString(Const Item: _TItem_): String;
Begin
  Unused(@Item);
  RaiseMethodNotRedefined;
  Result := '';
End;

{--- TGenTreeSet.Delete ---}
Procedure TGenTreeSet.Delete(Const Item: _TItem_);
Var
  C: TTreeMapCursor;
Begin
  C :=
    fMap.Find(Item);

  If C.IsNil Then
    RaiseItemNotInSet;

  fMap.DeleteAt(C);
End;

{--- TGenTreeSet.DeleteAt ---}
Procedure TGenTreeSet.DeleteAt(Const Position: TTreeSetCursor);
Begin
  fMap.
    DeleteAt(Position.Pos);
End;

{--- TGenTreeSet.DeleteFirst ---}
Procedure TGenTreeSet.DeleteFirst;
Begin
  fMap.DeleteFirst;
End;

{--- TGenTreeSet.DeleteLast ---}
Procedure TGenTreeSet.DeleteLast;
Begin
  fMap.DeleteLast;
End;

{--- TGenTreeSet.Destroy ---}
Destructor TGenTreeSet.Destroy;
Begin
  fMap.Free;
  Inherited;
End;

{--- TGenTreeSet.Difference ---}
Procedure TGenTreeSet.Difference(Left, Right: TGenTreeSet);
Begin
  If Left <> Self Then
  Begin
    Clear;
    IncludeAll(Left);
  End;

  If Left <> Right Then
    ExcludeAll(Right)
  Else
    Clear;
End;

{--- TGenTreeSet.EnumeratorGet ---}
Function TGenTreeSet.EnumeratorGet(Const Pos: TTreeSetCursor): _TItem_;
Begin
  ReadItemAt(
    Pos, Result);
End;

{--- TGenTreeSet.EnumeratorNext ---}
Function TGenTreeSet.EnumeratorNext(Var Pos: TTreeSetCursor): Boolean;
Begin
  If Pos.IsNil Then
    Pos := First
  Else
    Pos.MoveNext;
  Result :=
    Pos.HasItem;
End;

{--- TGenTreeSet.ExchangeContent ---}
Procedure TGenTreeSet.ExchangeContent(ASet: TGenTreeSet);
Var
  Tmp: TMap;
Begin
  Tmp := fMap;
  fMap := ASet.fMap;
  ASet.fMap := Tmp;
End;

{--- TGenTreeSet.GetOnCompareItems ---}
Function TGenTreeSet.GetOnCompareItems: TCompareItems;
Begin
  Result :=
    fMap.OnCompareKeys;
End;

{--- TGenTreeSet.GetOnItemToString ---}
Function TGenTreeSet.GetOnItemToString: TItemToString;
Begin
  Result :=
    fMap.OnKeyToString;
End;

{--- TGenTreeSet.Exclude ---}
Procedure TGenTreeSet.Exclude(Const Item: _TItem_);
Begin
  fMap.
    Exclude(Item);
End;

{--- TGenTreeSet.ExcludeAll ---}
Procedure TGenTreeSet.ExcludeAll(ASet: TGenTreeSet);
Var
  C: TTreeMapCursor;
  I: Integer;
Begin
  If ASet.GetSize > 0 Then
  Begin

    C :=
      ASet.fMap.First;
    For
      I := 1 To ASet.GetSize Do
    Begin
      Exclude(
        ASet.fMap.Keys[C]);
      C.MoveNext;
    End;
  End;
End;

{--- TTreeSetCursor ---}
Function TGenTreeSet.First: TTreeSetCursor;
Begin
  Result.
    Init(Self, fMap.First);
End;

{--- TGenTreeSet.FirstItem ---}
Function TGenTreeSet.FirstItem: _TItem_;
Begin
  fMap.
    ReadFirstKey(Result);
End;

{--- TGenTreeSet.Floor ---}
Function TGenTreeSet.Floor(Const Item: _TItem_): TTreeSetCursor;
Begin
  Result.
    Init(Self, fMap.Floor(Item));
End;

{--- TGenTreeSet.GetEnumerator ---}
Function TGenTreeSet.GetEnumerator: TEnumerator;
Begin
  Result :=
    TEnumerator.Create(fNilCursor, @EnumeratorNext, @EnumeratorGet);
End;

{--- TGenTreeSet.GetItemAt ---}
Function TGenTreeSet.GetItemAt(Const Position: TTreeSetCursor): _TItem_;
Begin
  fMap.
    ReadKeyAt(Position.Pos, Result);
End;

{--- TGenTreeSet.GetSize ---}
Function TGenTreeSet.GetSize: Integer;
Begin
  Result := fMap.Size;
End;

{--- TGenTreeSet.SetOnCompareItems ---}
Procedure TGenTreeSet.SetOnCompareItems(AValue: TCompareItems);
Begin
  If AValue = nil Then
    fMap.
      OnCompareKeys := @DefaultCompareItems
  Else
    fMap.
      OnCompareKeys := AValue;
End;

{--- TGenTreeSet.SetOnItemToString ---}
Procedure TGenTreeSet.SetOnItemToString(AValue: TItemToString);
Begin
  If AValue = nil Then
    fMap.
      OnKeyToString := @DefaultItemToString
  Else
    fMap.
      OnKeyToString := AValue;
End;

{--- TGenTreeSet.Include ---}
Procedure TGenTreeSet.Include(Const Item: _TItem_);
Begin
  fMap.
    Include(Item, 0);
End;

{--- TGenTreeSet.IncludeAll ---}
Procedure TGenTreeSet.IncludeAll(ASet: TGenTreeSet);
Var
  C: TTreeMapCursor;
  I: Integer;
Begin
  If ASet.GetSize > 0 Then
  Begin
    C :=
      ASet.fMap.First;

    For
      I := 1 To ASet.GetSize Do
    Begin
      Include(
        ASet.fMap.Keys[C]);
      C.MoveNext;
    End;
  End;
End;

{--- TGenTreeSet.Insert ---}
Procedure TGenTreeSet.Insert(Const Item: _TItem_);
Var
  Inserted: Boolean;
Begin
  Insert(
    Item, Inserted);
  If Not Inserted Then
    RaiseItemAlreadyInSet;
End;

{--- TGenTreeSet.Insert ---}
Procedure TGenTreeSet.Insert(Const Item: _TItem_; out Inserted: Boolean);
Begin
  fMap.
    Insert(Item, 0, Inserted);
End;

{--- TGenTreeSet.Intersection ---}
Procedure TGenTreeSet.Intersection(Left, Right: TGenTreeSet);
Var
  Inter, Tmp: TGenTreeSet;
  I: Integer;
  C: TTreeMapCursor;
  Item: _TItem_;
Begin
  If (Left.GetSize = 0) Or (Right.GetSize = 0) Then
    Clear
  Else
  Begin
    Inter :=
      TGenTreeSet.Create;
    Inter.
      OnCompareItems := OnCompareItems;
    Inter.
      OnItemToString := OnItemToString;

    Try
      If Left.GetSize < Right.GetSize Then
      Begin
        Tmp :=
          Left;
        Left :=
          Right;
        Right := Tmp;
      End;

      C :=
        Left.fMap.First;
      For
        I := 1 To Left.GetSize Do
      Begin
        Item :=
          Left.fMap.Keys[C];
        If Right.fMap.Contains(Item) Then
          Inter.
            Include(Item);
        C.MoveNext;
      End;

      ExchangeContent(
        Inter);
    Finally
      Inter.Free;
    End;
  End;
End;

{--- TGenTreeSet.IsEmpty ---}
Function TGenTreeSet.IsEmpty: Boolean;
Begin
  Result :=
    fMap.Size = 0;
End;

{--- TGenTreeSet.IsSubset ---}
Function TGenTreeSet.IsSubset(OfSet: TGenTreeSet): Boolean;
Var
  I: Integer;
  C: TTreeMapCursor;
Begin
  If GetSize > 0 Then
  Begin
    C :=
      fMap.First;
    For
      I := 1 To GetSize Do
    Begin
      If Not OfSet.fMap.Contains(fMap.Keys[C]) Then
      Begin
        Result :=
          False;
        Exit;
      End;
      C.MoveNext;
    End;
  End;
  Result := True;
End;

{--- TGenTreeSet.Last ---}
Function TGenTreeSet.Last: TTreeSetCursor;
Begin
  Result.
    Init(Self, fMap.Last);
End;

{--- TGenTreeSet.LastItem ---}
Function TGenTreeSet.LastItem: _TItem_;
Begin
  fMap.
    ReadLastKey(Result);
End;

{--- TGenTreeSet.Overlaps ---}
Function TGenTreeSet.Overlaps(ASet: TGenTreeSet): Boolean;
Var
  I: Integer;
  C: TTreeMapCursor;
Begin
  Result := False;
  If GetSize > 0 Then
  Begin
    C :=
      fMap.First;
    For
      I := 1 To GetSize Do
    Begin
      If ASet.fMap.Contains(fMap.Keys[C]) Then
      Begin
        Result := True;
        Break;
      End;
      C.MoveNext;
    End;
  End;
End;

{--- TGenTreeSet.ReadFirstItem ---}
Procedure TGenTreeSet.ReadFirstItem(out Value: _TItem_);
Begin
  fMap.
    ReadFirstKey(Value);
End;

{--- TGenTreeSet.ReadItemAt ---}
Procedure TGenTreeSet.ReadItemAt(Const Position: TTreeSetCursor; out Value: _TItem_);
Begin
  fMap.ReadKeyAt(Position.Pos, Value);
End;

{--- TGenTreeSet.ReadLastItem ---}
Procedure TGenTreeSet.ReadLastItem(out Value: _TItem_);
Begin
  fMap.
    ReadLastKey(Value);
End;

{--- TGenTreeSet.SymmetricDifference ---}
Procedure TGenTreeSet.SymmetricDifference(Left, Right: TGenTreeSet);
Var
  Inter: TGenTreeSet;
Begin
  Inter :=
    TGenTreeSet.Create;
  Inter.
    OnCompareItems := OnCompareItems;
  Inter.
    OnItemToString := OnItemToString;
  Try
    Inter.
      Intersection(Left, Right);
    Union(
      Left, Right);
    Difference(
      Self, Inter);
  Finally
    Inter.Free;
  End;
End;

{--- TGenTreeSet.ToString ---}
Function TGenTreeSet.ToString: String;
Var
  C: TTreeMapCursor;
Begin
  Result := '{';

  If GetSize > 0 Then
  Begin
    C :=
      fMap.First;
    While
      C.HasItem Do
    Begin
      Result :=
        Result + fMap.OnKeyToString(fMap.Keys[C]);
      If Not C.IsLast Then
        Result :=
          Result + '; ';
      C.
        MoveNext;
    End;
  End;

  Result :=
    Result + '}';
End;

{--- TGenTreeSet.Union ---}
Procedure TGenTreeSet.Union(Left, Right: TGenTreeSet);
Begin
  If Left <> Self Then
  Begin
    Clear;
    IncludeAll(Left);
  End;

  If Left <> Right Then
    IncludeAll(Right);
End;

End.
