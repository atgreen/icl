Name:           icl
Version:        0.1.0
Release:        1%{?dist}
Summary:        Interactive Common Lisp REPL

License:        MIT
URL:            https://github.com/atgreen/icl
Source0:        icl-%{version}.tar.gz

# Disable debug packages and stripping since this is a Lisp binary with dumped image
%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

BuildRequires:  sbcl
BuildRequires:  libfixposix-devel
BuildRequires:  gcc
BuildRequires:  make

%description
icl is an enhanced Common Lisp REPL. It provides a
modern command-line experience with line editing, inline documentation, and
helper commands to explore and evaluate Common Lisp code quickly.

%prep
%autosetup

%build
# Dependencies are vendored in the source tarball
# Build the executable
make

%install
# Install the binary
install -D -m 0755 icl %{buildroot}%{_bindir}/icl

# Install shared libraries (libosicat.so) - find in ASDF cache
mkdir -p %{buildroot}%{_libdir}/icl
find ~/.cache/common-lisp -name "libosicat.so" -exec install -m 0755 {} %{buildroot}%{_libdir}/icl/ \; 2>/dev/null || true

# Install ldconfig configuration
mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
echo "%{_libdir}/icl" > %{buildroot}%{_sysconfdir}/ld.so.conf.d/icl.conf

# Install bundled ASDF (for Lisps that don't bundle it, like CLISP)
mkdir -p %{buildroot}%{_datadir}/icl/asdf
cp 3rd-party/asdf/asdf.lisp %{buildroot}%{_datadir}/icl/asdf/

# Install Emacs integration
mkdir -p %{buildroot}%{_datadir}/emacs/site-lisp/icl
install -m 0644 emacs/icl.el %{buildroot}%{_datadir}/emacs/site-lisp/icl/
install -m 0644 emacs/icl-autoloads.el %{buildroot}%{_datadir}/emacs/site-lisp/icl/

# Install bash completion
mkdir -p %{buildroot}%{_datadir}/bash-completion/completions
install -m 0644 completions/icl.bash %{buildroot}%{_datadir}/bash-completion/completions/icl

# Install man page
mkdir -p %{buildroot}%{_mandir}/man1
install -m 0644 man/icl.1 %{buildroot}%{_mandir}/man1/

# Note: Slynk and browser assets are now embedded in the binary

# Install combined open source notices (web + Lisp licenses)
install -D -m 0644 assets/OPEN-SOURCE-NOTICES.txt %{buildroot}%{_datadir}/licenses/%{name}/OPEN-SOURCE-NOTICES.txt

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
%license LICENSE
%license %{_datadir}/licenses/%{name}/OPEN-SOURCE-NOTICES.txt
%doc README.md
%{_sysconfdir}/ld.so.conf.d/icl.conf
%{_datadir}/icl/asdf
%{_datadir}/emacs/site-lisp/icl
%{_datadir}/bash-completion/completions/icl
%{_mandir}/man1/icl.1*
%{_libdir}/icl
%{_bindir}/icl

%changelog
* Thu Dec 26 2025 Anthony Green <green@moxielogic.com> - 0.1.0-3
- Use combined OPEN-SOURCE-NOTICES.txt for all third-party licenses

* Sat Dec 13 2025 Anthony Green <green@moxielogic.com> - 0.1.0-2
- Collect vendored licenses during build and install them under /usr/share/licenses

* Sat Dec 13 2025 Anthony Green <green@moxielogic.com> - 0.1.0-1
- Initial RPM package for icl
