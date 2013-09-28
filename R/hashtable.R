#
# hashtable.R
# This file is part of hashtable
#
# Copyright (C) 2013 - Giacomo Bergami
#
# hashtable is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# hashtable is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with hashtable. If not, see <http://www.gnu.org/licenses/>.
#

require(digest)

IdRM_AddElem <- function (x,val) {
    return(addKeyValueElem(x,digest(val,algo="sha1"),val))
}

IdRM_RemElem <- function (x,val) {
    return(remKeyValueElem(x,digest(val,algo="sha1"),val))
}

IdRM_UpdateElem <- function (x,old,new) {
    IdRM_RemElem(x,old)
    return(IdRM_AddElem(x,new))
}