"""
Virtual Database that acts as an abstraction to the actual database.
VDB is the python representation of the on disk database.
VDB exposes methods to read/edit the database.
VDB can be serialized/deserialized to on disk db.
"""

import datetime
import re
from enum import Enum, auto
from typing import List, Optional, Tuple

import yaml

# from libgravatar import Gravatar  # type: ignore


class Visibility(Enum):
    """
    Enum to represent visibility levels for a post
    """
    Aham = auto()  # only visible to the author
    Gram = auto()  # visible to all logged in users
    Lok = auto()  # visible to everyone without log in


class VDB:
    """
    Python abstraction of panchayat DB
    """

    # pylint: disable=too-few-public-methods
    def __init__(self, outfile: str = None):
        self.users = UserList()
        self.posts = PostTree()
        self.outfile = outfile

    def commit(self):
        """
        serialize the virtual database to disk overwriting existing file
        """
        if not self.outfile:
            raise RuntimeError("Outfile is empty")

        with open(self.outfile, 'w') as outfile:
            yaml.dump(self, outfile)

        # git commit


class User:
    """
    Class to represent a user on panchayat
    """

    # pylint: disable=too-few-public-methods
    def __init__(self,
                 username: str,
                 password: str,
                 token: str = None,
                 email: str = None,
                 email_updates: bool = False):
        # pylint: disable=too-many-arguments
        self.username = username  # primary key
        self.password = password  #hash
        self.token = token
        self.email = email
        self.email_updates = email_updates

    def __str__(self) -> str:
        return self.username

    def gravatar_url(self) -> str:
        """
        Return gravatar image url for the user.
        If user has email, then email is used to generate image.
        Else username is used to generate image.
        """
        key = self.email if self.email else self.username
        return key
        # libgrav = Gravatar(key)
        # return libgrav.get_image(size=200, default="identicon", use_ssl=True)


class UserList(list):
    """
    List of users
    """
    def find(self, username: str) -> Optional[User]:
        """
        Find user by username
        """
        user = [user for user in self if user.username == username]
        if not user:
            return None
        if len(user) != 1:
            raise RuntimeError("More than one user found for username")
        return user[0]


class Post:  # pylint: disable=too-many-instance-attributes
    """
    Class to represent a post on panchayat.
    Inherited by LinkPost and TextPost
    """
    def __init__(
        self,
        author: User,
        title: str,
        body: str,
        visibility: Visibility = Visibility.Gram,
        upvotes=None,
        downvotes=None,
        created=None,
        parent: "Post" = None,
        post_id: int = None,
    ):
        # pylint: disable=too-many-arguments
        self.post_id = post_id  # need id for permalink
        self.author = author
        self.created = created \
            if created is not None else datetime.datetime.now()
        self.title = title
        self.body = body
        self.upvotes = upvotes if upvotes else set()
        self.downvotes = downvotes if downvotes else set()
        self.children: List[Post] = []
        self.parent = parent
        self.depth: int = parent.depth + 1 if parent else 0

        if (self.parent and self.parent.visibility == visibility.Aham
                and self.parent.author != self.author):
            raise RuntimeError("Cannot reply to someone else's aham post")
        self.visibility = visibility  # set visibility using setter

    @property
    def target_visibility(self):
        """
        Getter method for visibility
        """
        return self._visibility

    @property
    def visibility(self):
        """
        Getter method for visibility

        Visibility can be lower than target if some ancestor has lower visibility.
        When the ancestor reaches the requested target visibility,
        self will automatically reach target visibility as well.
        """
        if self.parent and self.parent.visibility.value < self._visibility.value:
            return self.parent.visibility
        return self._visibility

    @visibility.setter
    def visibility(self, other: Visibility):
        """
        Setter method for visibility
        If self is being made aham then parent and all descendants must be by same author
        While setting visibility, all descendants are capped to self visibility level

        This setter sets _visibility property. This sets the target visibility.
        But, the actual visibility can stay lower if some ancestor has lower visibility.
        """
        if other == Visibility.Aham:
            if any([
                    descendant.author != self.author
                    for descendant in self.descendants
            ]):
                raise RuntimeError(
                    "Cannot make post Aham if there are children owned by others"
                )

        self._visibility = other

    def visibility_detail_string(self):
        """
        The detailed string for visibility
        "(Visibility.name requested)" if some descendant has a higher target visibility
        "(Visibility.name pending)" if some ancestor is preventing this post from target visibility
        """
        ret = ''
        if self.target_visibility != self.visibility:
            ret += f'({self.target_visibility.name} pending)'
        if self.children:
            max_visibility_request = max([
                descendant.target_visibility for descendant in self.descendants
            ],
                                         key=lambda x: x.value)
            if max_visibility_request.value > self.target_visibility.value:
                ret += f'({max_visibility_request.name} requested)'
        return ret

    def is_visible_to(self, user: User = None) -> bool:
        """
        Returns True if self is visible to user, else False
        """
        if self.visibility == Visibility.Lok:
            return True
        if self.visibility == Visibility.Gram and user:
            return True
        if self.visibility == Visibility.Aham and self.author == user:
            return True
        return False

    @property
    def descendants(self) -> List["Post"]:
        """
        Return all my descendants with inorder traversal
        Does not include self
        """
        my_descendants = []  # list(self.children)
        for child in sorted(self.children, key=lambda post: post.created):
            my_descendants.append(child)
            my_descendants.extend(child.descendants)
        return my_descendants

    @property
    def family(self) -> List["Post"]:
        """
        Return list of posts in family
        Two posts belong to same family if they share the same TLP
        """
        return self.tlp.descendants_and_i

    @property
    def descendants_and_i(self) -> List["Post"]:
        """
        Return all my descendants with inorder traversal
        Includes self
        """
        return [self] + self.descendants

    @property
    def ancestors(self) -> List["Post"]:
        """
        Return all my ancestors oldest first
        Does not include self
        """
        if self.parent:
            return self.parent.ancestors + [self.parent]
        return []

    @property
    def ancestry(self) -> List["Post"]:
        """
        Return all my ancestors including self
        """
        if self.parent:
            return self.parent.ancestry + [self]
        return [self]

    @property
    def tlp(self) -> "Post":
        """
        Return my top level post
        """
        if self.is_tlp():
            return self
        return self.parent.tlp  #type: ignore

    def is_tlp(self) -> bool:
        """
        Return True if I am a top level post
        """
        return self.depth == 0

    def is_leaf(self) -> bool:
        """
        Return True if I am a leaf post
        """
        return not self.children

    @property
    def vote_count(self) -> int:
        """
        Return the effective vote count of this post. upvote - downvote
        """
        return len(self.upvotes) - len(self.downvotes)

    def upvote_string(self) -> str:
        """
        Return string of all users who have upvoted this post
        """
        return ', '.join([user.username for user in self.upvotes])

    def downvote_string(self) -> str:
        """
        Return string of all users who have downvoted this post
        """
        return ', '.join([user.username for user in self.downvotes])

    def __str__(self) -> str:
        if self.title:
            return self.title
        return self.body

    def nullvote(self, user: User):
        """
        Remove user's vote from this post
        """
        self.upvotes.discard(user)
        self.downvotes.discard(user)

    def upvote(self, user: User):
        """
        Upvote this post. Upvote is done by voiding previous vote and creating new one.
        """
        self.nullvote(user)
        self.upvotes.add(user)

    def downvote(self, user: User):
        """
        Downvote this post. Downvote is done by voiding previous vote and creating new one.
        """
        self.nullvote(user)
        self.downvotes.add(user)

    def delete(self):
        """
        Delete this post. Does not remove the post from db,
        but only overwrites title and body with 'DELETED'.
        This is done to not break other posts that have reference to the deleted one.
        """
        self.title = "DELETED"
        self.body = "DELETED"

    def family_last_modified(self) -> datetime.datetime:
        """
        Return when the post family was last modified
        Max of created for all posts in family
        """
        return max([post.created for post in self.family])


class LinkPost(Post):
    """
    Class to represent a link post on panchayat
    """
    def is_url(self) -> bool:  # pylint: disable=missing-function-docstring, no-self-use
        return True


class TextPost(Post):
    """
    Class to represent a text post on panchayat
    """
    def is_url(self) -> bool:  #pylint: disable=missing-function-docstring, no-self-use
        return False

    @property
    def html_body(self) -> str:
        """
        Return html string with all urls in body converted to hrefs
        Regex taken from https://urlregex.com/
        Trailing period and parenthesis was appended to remove false positives
        """
        # pylint: disable=line-too-long
        url_regex = re.compile(
            r'''(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+[^\. \)])'''
        )
        return url_regex.sub(r'<a href="\1" target="_blank">\1</a>', self.body)


class PostTree:
    """
    Class to represent a tree of posts
    """
    def __init__(self):
        self.tlps = []

    def zig_zag(self) -> List[Post]:
        """
        Return all posts in zig zag order.
        TLPs are in reverse chronological order.
        Comments are ordered chrnonologically.
        """
        all_posts = []
        reverse_chrono_tlps = sorted(self.tlps,
                                     key=lambda post: post.created,
                                     reverse=True)
        for tlp in reverse_chrono_tlps:
            all_posts.append(tlp)
            all_posts.extend(tlp.descendants)
        return all_posts

    def compressed_reverse_chrono_ancestry(
            self, requesting_user: User) -> List[Tuple[Post, bool, bool]]:
        """
        Returns a list of all posts with their ancestors.
        The post is attached to two boolean fields wrapped inside a tuple
        for use by the jinja template.
        First boolean indicates whether this post must be highlighted.
        Second boolean indicates whether a new TLP boundary has reached.
        Ancestry is not repeated when the subsequent post shares ancestors.
        This query is used in the activity view.
        """
        ret: List[Tuple[Post, bool, bool]] = []
        prev_ancestors: List[Post] = []
        prev_tlp: Optional[Post] = None
        for post in self.reverse_chrono():
            if not post.is_visible_to(requesting_user):
                continue
            if prev_tlp and post.tlp is not prev_tlp:
                # make tlp_switch true for the previous post
                ret[-1] = (ret[-1][0], ret[-1][1], True)
            ret.extend([(ancestor, False, False) for ancestor in post.ancestors
                        if ancestor not in prev_ancestors])
            # add current post with highlight true
            ret.append((post, True, False))
            prev_ancestors = post.ancestry
            prev_tlp = post.tlp
        return ret

    def all(self) -> List[Post]:
        """
        Return list of all posts in any order.
        Currently zig_zag order.
        """
        return self.zig_zag()

    def reverse_chrono(self) -> List[Post]:
        """
        Return all posts in reverse chronological order
        """
        return sorted(self.all(), key=lambda post: post.created, reverse=True)

    def find(self, post_id: int) -> Optional[Post]:
        """
        Find a post by post id
        """
        post = [post for post in self.all() if post.post_id == post_id]
        if not post:
            return None
        if len(post) != 1:
            raise RuntimeError(
                "There should only have been one post with a given id")
        return post[0]

    def insert(self, post: Post):
        """
        Insert a post into the posttree.
        If the post does not have an id already assign the smallest available one.
        If post has a parent add the post as child of parent.
        Else add the post as a TLP.
        """
        if post.post_id is None:
            post.post_id = max(  #type:ignore
                [post.post_id for post in self.all()],
                default=0) + 1  #type: ignore

        if self.find(post.post_id) is not None:
            raise RuntimeError("Posttree already contains post with id")

        if post.parent is None:
            self.tlps.append(post)
        else:
            post.parent.children.append(post)

    def tlp_count(self, user: User) -> int:
        """
        Return #TLPs by the user
        """
        return len([post for post in self.tlps if post.author == user])

    def comment_count(self, user: User) -> int:
        """
        Return #comments by user
        """
        return len([
            post for post in self.all()
            if post.depth != 0 and post.author == user
        ])

    def upvote_count(self, user: User) -> int:
        """
        Return #upvotes by user
        """
        return len([post for post in self.all() if user in post.upvotes])

    def downvote_count(self, user: User) -> int:
        """
        Return #downvotes by user
        """
        return len([post for post in self.all() if user in post.downvotes])

