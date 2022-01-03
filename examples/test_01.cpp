/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
// [!.. Licence ]
/* Vivamus ac  semper nibh. Sed  pretium est  id lobortis efficitur.  Nulla eget
 * metus at urna volutpat suscipit vitae at lectus. Fusce nibh lacus, eleifend a
 * mi  a, lobortis  tempor erat.  Cras luctus  nunc quam,  vitae posuere  tortor
 * rutrum id. Etiam eu velit vel  sem facilisis efficitur. Praesent non felis in
 * risus maximus fringilla.  */

// [!.. Info ]
// Cras  aliquet malesuada  eros, sit  amet sagittis  sapien commodo  et. Mauris
// egestas    arcu    pellentesque    est    tempus,    eget    accumsan    diam
// ultricies. Vestibulum sed commodo quam.  Vestibulum suscipit justo ac aliquet
// dignissim.  Phasellus  rutrum mi  vitae  tortor  commodo, vel  tempus  turpis
// elementum. Maecenas iaculis pulvinar efficitur. Mauris tincidunt suscipit mi,
// sed  bibendum nulla  lacinia sed.  Nunc sit  amet mi  varius, blandit  lectus
// vitae, tempor nisi.

// [!.. Code ]

// [!0B Includes0 ]
// [!1B Includes1 ]
// [!2B Includes2 ]
// [!3B Includes3 ]
// [!4B Includes4 ]
// [!5B Includes5 ]
// [!6B Includes6 ]
// [!7B Includes7 ]
// [!8B Includes8 ]
// [!9B Includes9 #5 test]
#include <assert.h>
#include <getopt.h>
#include <unistd.h>

#include <algorithm>
#include <numeric>
#include <vector>

//---------------------------------------------------------------------------
// Utilities [!3F]
//---------------------------------------------------------------------------

static const char* gArgv0;

// Utilities [!8F]

#  include <sys/types.h>
#  include <sys/sysctl.h>

//---------------------------------------------------------------------------
// Mac specific code [!1R]
//---------------------------------------------------------------------------

#if defined(__APPLE__)

#  include <sys/types.h>
#  include <sys/sysctl.h>

//---------------------------------------------------------------------------
// Linux specific code [!1R]

#  include <sys/types.h>
#  include <sys/sysctl.h>

// Linux specific code [!2R]
//---------------------------------------------------------------------------
static void printUsage() {
}

#elif defined(__linux__)

#  include <linux/perf_event.h>
#  include <sys/syscall.h>

#else

//---------------------------------------------------------------------------
// Unsupported platforms
//---------------------------------------------------------------------------

#  error Sorry, this platform is not supported

#endif  // platform

//---------------------------------------------------------------------------
// The main loop [!1R]
//---------------------------------------------------------------------------
static void printUsage() {
    ...
}

// [!.. Signals ]

static const char* gArgv0;
// [!2R SIGALRM ]
static void printUsage() {
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }
}

// [!2R SIGINT ]
static void printUsage() {
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }
}

// [!1.]
// [!7F PrintUsage ]
static void printUsage() {
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }
}

// [!2F Main #3 test]
int main(int argc, char** argv) {
  // Process command line options. [!5F]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

  // // [!.. // Parse options. ]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

  // [!3R The update. ]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

  // Initialize something. [!2.# test]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

  // Install the signal handlers. [!+B]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

// [!>2 Lorem ipsum dolor sit amet, consectetur adipiscing elit. ]

// [!.. Sed vel condimentum elit, in pulvinar lectus. Mauris dictum felis sodales est venenatis. ]
// [!1F Suspendisse eu risus pretium, porta nisi vel, feugiat nisl. Nam sodales tincidunt turpis. ]
// Donec neque risus, dapibus eleifend volutpat [!..]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

/*
  Suspendisse eu risus pretium, porta nisi vel, feugiat nisl.
 */

// [!1F In ante sapien, sodales a semper eget, pharetra eget leo. In aliquam gravida nibh. ]
// Cras eu ipsum commodo, vestibulum erat sed, accumsan diam. [!..]
void
 doSomethingFromCache( char const * binPath )
{
    // [!2F n eu luctus nisl. Mauris mollis justo vitae libero iaculis malesuada. ]
    // [!.. Nunc augue ex, ultricies ac lorem a, placerat bibendum diam. ]
    int fd = open( binPath, O_RDONLY );

    // [!3? # test]
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }

    // [!+F Mauris mauris magna, molestie sit amet quam et, ornare lacinia enim. ]
    unsigned char * header[ HEADER_SIZE ];

    // Below just adds a link, like a "see also".
    // [!.C Pellentesque luctus magna sem, id commodo libero auctor vel. ]

    // [!7? Morbi tempor turpis gravida, dignissim sem sit amet, feugiat justo. ]
    // Ensure cache is valid.
    if ( meta.vPerThing )
    {
        // [!.. Sed nisl arcu, interdum a pellentesque sagittis, finibus ac tortor. ]
        switch ( meta.unitSize )
        {
            case 8:
                // Quisque leo enim, porta vitae elit at, euismod placerat ipsum. [!2F]
                // [!.. Duis aliquet arcu ut odio iaculis, sagittis venenatis sapien lacinia. ]
                // [!.. Ut porttitor tellus vel sagittis blandit. ]
                // Below turns jump into the "inner frame".
                // [!>. Jump ]

                // Optionally, it can contain a significance/complexity hint about jump target.
                // [!4C Begin ]
                // [!>2 Middle ]
                // [!<F End ]
                doSomething< double >( fd, meta );
                break;
        }
    }

    // [!3. Vivamus libero massa, malesuada ac nisi et, placerat blandit leo. ]
    // [!.. Curabitur eget consequat lacus. ]
    // [!+F Pellentesque nec ipsum vitae nibh congue aliquet at sit amet dolor. ]
    close( fd );
}
// Maecenas odio eros, pulvinar quis gravida quis, egestas tincidunt nibh. [!0.]

// [!0F Class aptent taciti sociosqu ]



// [!>2 Nunc eros justo, semper vel mattis non, semper in turpis. ]

// [!.. Morbi tempor turpis gravida, dignissim sem ]
// [!1F Lorem ipsum. ]
// Vivamus nec molestie purus. [!..]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

/*
  Suspendisse eu risus pretium, porta nisi vel, feugiat nisl.
 */

// [!1F Lorem ipsum dolor sit amet. ]
// Donec mollis aliquam sem eget finibus. [!..]
void
 doSomethingFromCache( char const * binPath )
{
    // [!2F Vivamus porttitor sollicitudin dui, ac aliquam mi commodo vel. ]
    // [!.. Pellentesque nec ipsum vitae nibh congue aliquet at sit amet dolor. ]
    int fd = open( binPath, O_RDONLY );

    // [!3? Duis a metus id erat molestie bibendum eu a mauris. ]
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }
