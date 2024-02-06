;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells)
             (gnu home services ssh)
             (gnu home services))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "arandr"
                                            "xrandr"
                                            "emacs-pdf-tools"
                                            "libpng"
                                            "automake"
                                            "autoconf"
                                            "libvterm"
                                            "libtool"
                                            "gcc-toolchain@11"
                                            "make"
                                            "coreutils"
                                            "cmake"
                                            "curl"
                                            "stow"
                                            "unzip"
                                            "git"
                                            "firefox")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (bashrc (list (local-file
                                  "/home/ross/.dotfiles/stow/bash/.bashrc" "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/ross/.dotfiles/stow/bash/.bash_profile"
                                        "bash_profile")))))
         (simple-service 'dotfile-symlinks
                         home-files-service-type
                         (list `(".env" ,(local-file "/home/ross/.dotfiles/stow/bash/.env" "env"))))
         (service home-openssh-service-type (home-openssh-configuration (add-keys-to-agent "yes"))))))
