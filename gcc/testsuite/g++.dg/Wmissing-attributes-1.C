// { dg-do compile }
// { dg-options "-Wmissing-attributes" }

#define ATTR(list)   __attribute__ (list)

/* Type attributes are normally absent in template functions, and the
   mere presence of any such attribute used to cause the
   -Wmissing-attributes checks, that checked for attributes typically
   associated with functions rather than types, to report any missing
   attributes twice: once for the specialization attribute list, once
   for its type attribute list.

   If we force any of the checked-for attributes to be associated with
   types rather than functions, even later implementations that fixed
   the duplicate reporting problem above would still report them as
   missing (in the function attribute list) even when present (in the
   type attribute list).  */
typedef void* ATTR ((alloc_size (1))) f_type (int);

template <class T>
f_type
ATTR ((malloc))
missing_malloc;            // { dg-message "missing primary template attribute .malloc." }

template <>
f_type
missing_malloc<char>;      // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }


/* Check that even an attribute that appears in both lists (decl and
   type) in a template declaration is reported as missing only
   once.  */

template <class T>
f_type
ATTR ((alloc_size (1))) // In both attr lists, decl's and type's.
missing_alloc_size;            // { dg-message "missing primary template attribute .alloc_size." }

template <>
void *
missing_alloc_size<char>(int); // { dg-warning "explicit specialization .\[^\n\r\]+. may be missing attributes" }


/* Check that even an attribute that appears in both lists (decl and
   type) is not report as missing if it's present only in the type
   list.  */

template <class T>
f_type
ATTR ((alloc_size (1))) // In both attr lists, decl's and type's.
missing_nothing;

template <>
f_type
missing_nothing<char>;
