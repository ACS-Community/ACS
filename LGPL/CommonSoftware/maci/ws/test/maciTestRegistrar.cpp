/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestRegistrar.cpp,v 1.78 2003/01/16 12:14:29 vltsccm Exp $"
*
* who       when       what
* --------  ---------- ----------------------------------------------
* msekoran  2002-07-08 created
*/

static char *rcsId="@(#) $Id: maciTestRegistrar.cpp,v 1.78 2003/01/16 12:14:29 vltsccm Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciRegistrar.h>
#include <stdio.h>

const char *registrarStates[] = {
    "The registrar is in a consistent state.",                                              // 0
    "Doubly-linked list exceeds the capacity of the registrar;"                             // 1
    "some of its elements are not within the array managed by the registrar.",              
    "Doubly-linked list is inconsistent (element's next's previous is not the element).",   // 2
    "An element is in the free element chain, but it  is marked as allocated.",             // 3
    "An  element in the allocated element chain, but it is marked as free.",                // 4
    "The two doubly-linked lists don't span the entire capacity of the registrar."          // 5
};

#define CHECK(reg) \
    { \
        int result = reg.CheckIntegrity(); \
        printf("CHECK: %s\n", registrarStates[result]); \
    }

#define ALLOCATE_TEST(h, reg) \
    int h = reg.Allocate(); \
    reg[h] = h;

#define PREALLOCATE_TEST(h, reg) \
    int h = reg.Preallocate(); \
    reg[h] = h;

#define DUMP(reg) \
    { \
        int h = reg.First(); \
        while (h != 0) \
        { \
          printf("%d ", reg[h]); \
	      h = reg.Next(h); \
        } \
        printf("\n"); \
    } 



int main(int argc, char *argv[])
{
    Registrar<int, int> registry;

    CHECK(registry);
    
    ALLOCATE_TEST(h1, registry)
    ALLOCATE_TEST(h2, registry)
    ALLOCATE_TEST(h3, registry)

    registry.Deallocate(h2);

    CHECK(registry);
    DUMP(registry)

    PREALLOCATE_TEST(h4, registry)
    registry.AckAllocate(h4);

    CHECK(registry);
    DUMP(registry)

    PREALLOCATE_TEST(h5, registry)
    registry.Depreallocate(h5);

    CHECK(registry);
    DUMP(registry)


    PREALLOCATE_TEST(h6, registry)
    PREALLOCATE_TEST(h7, registry)


    ALLOCATE_TEST(h8, registry)
    ALLOCATE_TEST(h9, registry)
    ALLOCATE_TEST(h10, registry)

    registry.Deallocate(h10);
    registry.AckAllocate(h7);
    registry.Depreallocate(h6);

    CHECK(registry);
    DUMP(registry)

    PREALLOCATE_TEST(h11, registry)

    int i = registry.First();
    while (i) i = registry.Deallocate(i);

    registry.Depreallocate(h11);

    CHECK(registry);
    DUMP(registry)


    PREALLOCATE_TEST(h12, registry)
    ALLOCATE_TEST(h13, registry)
    registry.Depreallocate(h12);
 
    CHECK(registry);
    DUMP(registry)

    registry.Deallocate(13);

    PREALLOCATE_TEST(h14, registry)
    ALLOCATE_TEST(h15, registry)
    registry.AckAllocate(h14);
 
    CHECK(registry);
    DUMP(registry)

    registry.Deallocate(h14);
    registry.Deallocate(h15);

    PREALLOCATE_TEST(h20, registry)
    PREALLOCATE_TEST(h21, registry)
    PREALLOCATE_TEST(h22, registry)
    registry.AckAllocate(h20);
    PREALLOCATE_TEST(h23, registry)
    registry.AckAllocate(h22);
    PREALLOCATE_TEST(h24, registry)
    PREALLOCATE_TEST(h25, registry)
    PREALLOCATE_TEST(h26, registry)
    registry.Depreallocate(h21);
    PREALLOCATE_TEST(h27, registry)
    PREALLOCATE_TEST(h28, registry)
    PREALLOCATE_TEST(h29, registry)

    registry.AckAllocate(h23);
    registry.AckAllocate(h24);
    registry.AckAllocate(h26);
    registry.Depreallocate(h25);
    registry.AckAllocate(h28);
    registry.Depreallocate(h27);
    registry.AckAllocate(h29);

    CHECK(registry);
    DUMP(registry)

/*
    // unalloved case (should fail)
    ALLOCATE_TEST(h30, registry)
    registry.AckAllocate(h30);

    CHECK(registry);
    // this crashes DUMP(registry)
*/
    return 0;
}
