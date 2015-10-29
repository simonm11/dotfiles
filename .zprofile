#open jdk (with infinality font fix) for intellij
export IDEA_JDK=/usr/lib/jvm/java-7-openjdk/
export WEBIDE_JDK=/usr/lib/jvm/java-7-openjdk/
export STUDIO_JDK=/usr/lib/jvm/java-7-openjdk/
export PATH=~/npm-global/bin:$PATH

# start x on login
[[ $(fgconsole 2) == 1 ]] && exec startx -- vt1


