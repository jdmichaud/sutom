const std = @import("std");
const Regex = @import("regex").Regex;
const Linenoise = @import("linenoise").Linenoise;
const ziglyph = @import("ziglyph");

fn replace_accented_char(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
  var normalizer = try ziglyph.Normalizer.init(allocator);
  defer normalizer.deinit();

  var clean_buffer = try allocator.dupe(u8, str);

  var str_nfd = try normalizer.nfd(allocator, str);
  defer str_nfd.deinit();

  var i: usize = 0;
  for (str_nfd.slice) |c| {
    // std.debug.print("{} {c}\n", .{ c, c });
    if (c < 0x7F) {
      clean_buffer[i] = c;
      i += 1;
    }
  }

  _ = allocator.resize(clean_buffer, i);
  clean_buffer = clean_buffer[0..i];
  return clean_buffer;
}

fn loadfile(allocator: std.mem.Allocator, path: []const u8) !struct { buffer: []u8, wordlist: [][]const u8 } {
  var file = try std.fs.cwd().openFile(path, .{});
  defer file.close();

  const file_size: usize = @intCast(try file.getEndPos());
  const buffer = std.os.mmap(
      null,
      file_size,
      std.os.PROT.READ,
      std.os.MAP.SHARED,
      file.handle,
      0,
  ) catch return error.OutOfMemory;
  defer std.os.munmap(buffer);

  // clean_buffer contains only non accented upper case letters.
  var clean_buffer = try replace_accented_char(allocator, buffer);
  clean_buffer = std.ascii.upperString(clean_buffer, clean_buffer);
  // std.debug.print("{s}", .{ clean_buffer });
  var wordlist = std.ArrayList([]const u8).init(allocator);
  defer wordlist.deinit();

  var it = std.mem.splitScalar(u8, clean_buffer, '\n');
  while (it.next()) |word| {
    if (word.len >= 6 and word.len <= 9) {
      try wordlist.append(word);
    }
  }

  return .{ .buffer = clean_buffer, .wordlist = try wordlist.toOwnedSlice() };
}

const LetterContraint = struct {
  in: ?[]u8,
  out: std.ArrayList(u8),
};

const WordContraints = struct {
  letter_in: std.ArrayList(u8),
  letter_out: std.ArrayList(u8),
  regex: []const u8,
  allocator: std.mem.Allocator,

  const Self = @This();

  fn init(allocator: std.mem.Allocator, letter_in: std.ArrayList(u8), letter_out: std.ArrayList(u8), regex: []const u8) Self {
    return Self {
      .letter_in = letter_in,
      .letter_out = letter_out,
      .regex = regex,
      .allocator = allocator,
    };
  }

  fn deinit(self: *Self) void {
    self.letter_out.deinit();
    self.letter_in.deinit();
    self.allocator.free(self.regex);
  }
};

fn create_regex(allocator: std.mem.Allocator, inputs: std.ArrayList([]const u8), last_propositions: [][]const u8) !WordContraints {
  std.debug.assert(inputs.items.len > 0);
  // The letters that should appear somewhere in the proposition
  var letter_in = std.ArrayList(u8).init(allocator);
  // The letters that should not appear in the proposition
  var letter_out = std.ArrayList(u8).init(allocator);
  // The known contraints on each letter which will be converted to a regex
  var letter_contraints = try allocator.alloc(LetterContraint, inputs.items[0].len);
  // initialization of letter_contraints
  {
    var i: usize = 0;
    while (i < letter_contraints.len) {
      letter_contraints[i] = .{
        .in = null,
        .out = std.ArrayList(u8).init(allocator),
      };
      i += 1;
    }
  }
  // deinitialization of letter_contraints
  defer {
    var i: usize = 0;
    while (i < letter_contraints.len) {
      letter_contraints[i].out.deinit();
      i += 1;
    }
    allocator.free(letter_contraints);
  }

  // Based on the provided pattern by the user, add/remove constraints
  for (inputs.items[0..], 0..) |input, input_index| {
    for (input, 0..) |c, index| {
      switch (c) {
        '.' => {
          try letter_out.append(last_propositions[input_index][index]);
          try letter_contraints[index].out.append(last_propositions[input_index][index]);
        },
        'R' => {
          // this is for excluing those from letter_our
          try letter_in.append(last_propositions[input_index][index]);
          if (letter_contraints[index].in == null) {
            letter_contraints[index].in = try std.fmt.allocPrint(allocator, "{c}",
              .{ last_propositions[input_index][index] });
          }
          letter_contraints[index].out.clearAndFree();
        },
        'J' => {
          try letter_in.append(last_propositions[input_index][index]);
          try letter_contraints[index].out.append(last_propositions[input_index][index]);
        },
        else => unreachable,
      }
    }
  }

  // remove from letter_out letter in letter_in
  // Because Yellows are reported only the number of time a letter is present,
  // thus you can end up with a letter marked with a '.' even though it was
  // already flagged with a 'J'.
  var i: usize = 0;
  while (i < letter_out.items.len) {
    if (findIndex(u8, letter_in, letter_out.items[i]) != null) {
      _ = letter_out.orderedRemove(i);
    } else {
      i += 1;
    }
  }

  // Now build the regular expression from the letter_constraints
  var regexElements = std.ArrayList([]const u8).init(allocator);
  defer {
    for (regexElements.items) |elt| {
      allocator.free(elt);
    }
    regexElements.deinit();
  }
  for (letter_contraints) |letter_contraint| {
    try regexElements.append(letter_contraint.in orelse
      if (letter_contraint.out.items.len > 0) toto: {
        var letter_list = try allocator.alloc(u8, letter_contraint.out.items.len + 3);
        letter_list[0] = '['; letter_list[1] = '^'; letter_list[letter_list.len - 1] = ']';
        for (letter_contraint.out.items, 0..) |letter, letter_index| {
          letter_list[letter_index + 2] = letter;
        }
        break :toto letter_list;
      } else try allocator.dupe(u8, "."));
  }

  const regex = try std.mem.concat(allocator, u8, regexElements.items);
  // std.debug.print("regex: {s}\n", .{ regex });
  return WordContraints.init(allocator, letter_in, letter_out, regex);
}

fn findIndex(comptime T: type, list: std.ArrayList(T), term: T) ?usize {
  return for (list.items, 0..) |entry, index| {
    switch (@typeInfo(T)) {
      .Pointer => |info| if (std.mem.eql(info.child, entry, term)) break index,
      else => if (entry == term) break index,
    }
  } else null;
}

// Return true is all the characters in the list are present in the word
fn findAll(list: std.ArrayList(u8), word: []const u8) bool {
  for (list.items) |c| {
    var found = false;
    for (word) |w| {
      found = w == c;
      if (found) break;
    }
    if (!found) {
      return false;
    }
  }
  return true;
}

// Returns true if the letters in the list are not present in the word except
// for already found letters (R in the regex)
fn findNone(list: std.ArrayList(u8), word: []const u8, regex: []const u8) bool {
  for (list.items) |c| {
    for (word, 0..) |w, i| {
      if (w == c and regex[i] != 'R') {
        return false;
      }
    }
  }
  return true;
}

fn propose(allocator: std.mem.Allocator, word_constraints: WordContraints,
  wordlist: [][]const u8, propositions: std.ArrayList([]const u8), size: usize) !?[]const u8 {

  var regex = try Regex.compile(allocator, word_constraints.regex);
  defer regex.deinit();
  var i: u32 = 0;
  while (i < wordlist.len and (
    !(wordlist[i].len == size) or // Because of https://github.com/tiehuis/zig-regex/issues/11
    !findNone(word_constraints.letter_out, wordlist[i], word_constraints.regex) or
    !findAll(word_constraints.letter_in, wordlist[i]) or
    !try regex.match(wordlist[i]) or
    findIndex([]const u8, propositions, wordlist[i]) != null
  )) {
    i += 1;
  }
  // std.debug.print("propositions {s}\n", .{ propositions.items });
  // std.debug.print("wordlist[i] {s}\n", .{ wordlist[i] });
  // std.debug.print("findIndex {any}\n", .{ findIndex([]const u8, propositions, wordlist[i]) });
  if (i >= wordlist.len) {
    return null;
  }
  return wordlist[i];
}

fn check(allocator: std.mem.Allocator, word: []const u8, proposition: []const u8) ![]u8 {
  std.debug.assert(word.len == proposition.len);

  var response: []u8 = try allocator.dupe(u8, word);
  var i: usize = 0;
  // word_minus_R will contain word minus the already found letters
  // We will use it as a mask to flag the Yellow (J)
  var word_minus_R: []u8 = try allocator.dupe(u8, word);
  defer allocator.free(word_minus_R);
  var counters = [_]u8{ 0 } ** 128;

  while (i < word.len) {
    counters[word[i]] += 1;
    response[i] = '.';
    if (word[i] == proposition[i]) {
      counters[word[i]] -= 1;
      response[i] = 'R';
      word_minus_R[i] = '.';
    }
    i += 1;
  }
  // Now flag the Yellow (J)
  i = 0;
  while (i < word.len) {
    if (response[i] == 'R') {
      i += 1;
      continue;
    }
    if (counters[proposition[i]] > 0 and std.mem.indexOfAny(u8, word_minus_R, &[_]u8 { proposition[i] }) != null) {
      counters[proposition[i]] -= 1;
      response[i] = 'J';
    }
    i += 1;
  }
  return response;
}

pub fn main() !void {
  var gpa = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = gpa.allocator();
  defer _ = gpa.deinit();

  var stdout = std.io.getStdOut().writer();

  var arg_it = std.process.args();
  _ = arg_it.skip();
  const to_destructure = try loadfile(allocator, try (arg_it.next() orelse error.InvalidArgs));
  const wordlist = to_destructure.wordlist;
  const buffer = to_destructure.buffer;
  defer allocator.free(buffer);
  defer allocator.free(wordlist);

  try stdout.print("{} words loaded\n", .{ wordlist.len });

  var ln = Linenoise.init(allocator);
  defer ln.deinit();

  // Uncomment to automate testing
  // const TestCase = struct {
  //   word: []const u8,
  //   seed: []const u8
  // };
  // const tests = [_]TestCase {
  //   TestCase{ .word = "TECHNIQUE", .seed = "T........" },
  //   TestCase{ .word = "TURQUOISE", .seed = "T........" },
  //   TestCase{ .word = "BUSINESS", .seed = "B......." },
  //   TestCase{ .word = "PEPLUM", .seed = "P....." },
  // };
  // for (tests) |t| {
  //   var input = try allocator.dupe(u8, t.seed);

  try stdout.print(".: La lettre n'est pas present\n", .{});
  try stdout.print("J: La lettre est presente ailleurs\n", .{});
  try stdout.print("R: La lettre est bien positionee\n", .{});

  var propositions = std.ArrayList([]const u8).init(allocator);
  defer propositions.deinit();
  var inputs = std.ArrayList([]const u8).init(allocator);
  defer {
    for (inputs.items) |i| { allocator.free(i); }
    inputs.deinit();
  }

  // Uncomment to automate testing
  // while (true) {
  //   std.debug.print("? {s}\n", .{ input });
  while (try ln.linenoise("? ")) |input| {

    const upper_input = try allocator.dupe(u8, input);
    _ = std.ascii.upperString(upper_input, input);
    allocator.free(input);
    // Check if we found the word
    var only_red = false;
    for (upper_input) |c| {
      only_red = c == 'R';
      if (!only_red) break;
    }
    if (only_red) {
      try stdout.print("Trouve en {} essais\n", .{ propositions.items.len });
      allocator.free(upper_input);
      break;
    }


    if (upper_input.len < 6 or upper_input.len > 9) {
      try stdout.print("Le mot doit etre constitue de 6 a 9 lettres; {} lettres fournies. \n", .{ input.len });
      continue;
    }

    var word_constraints = if (propositions.items.len > 0) toto: {
      try inputs.append(upper_input);
      const word_constraints = try create_regex(allocator, inputs, propositions.items);
      break :toto word_constraints;
    } else toto: {
      break :toto WordContraints.init(
        allocator,
        std.ArrayList(u8).init(allocator),
        std.ArrayList(u8).init(allocator),
        upper_input,
      );
    };
    defer word_constraints.deinit();

    const pproposition = try propose(allocator, word_constraints, wordlist, propositions, upper_input.len);
    if (pproposition) |proposition| {
      try stdout.print("> {s}\n", .{ proposition });
      try propositions.append(proposition);
  // Uncomment to automate testing
      // input = try check(allocator, t.word, proposition);
    } else {
      try stdout.print("Aucun mot trouve\n", .{});
      break;
    }
  }
  // Uncomment to automate testing
  // }
}

test "findIndex" {
  var propositions = std.ArrayList([]const u8).init(std.testing.allocator);
  defer propositions.deinit();

  try propositions.append("test");
  try std.testing.expectEqual(
    findIndex([]const u8, propositions, "toto"),
    null
  );
  try std.testing.expectEqual(
    findIndex([]const u8, propositions, "test"),
    0
  );
}

test "findAll" {
  var list = std.ArrayList(u8).init(std.testing.allocator);
  defer list.deinit();
  try list.append('a');
  try list.append('b');
  try std.testing.expectEqual(findAll(list, "qwcdew"), false);
  try std.testing.expectEqual(findAll(list, "qwadew"), false);
  try std.testing.expectEqual(findAll(list, "qwabew"), true);
}

test "findNone" {
  var list = std.ArrayList(u8).init(std.testing.allocator);
  defer list.deinit();
  try list.append('a');
  try list.append('b');
  try std.testing.expectEqual(findNone(list, "qwcdew", "......"), true);
  try std.testing.expectEqual(findNone(list, "qwadew", "......"), false);
  try std.testing.expectEqual(findNone(list, "qwabew", "......"), false);
  try std.testing.expectEqual(findNone(list, "arfjhy", "R.JJ.J"), true);
  try std.testing.expectEqual(findNone(list, "aafjhy", "R.JJ.J"), false);
  try std.testing.expectEqual(findNone(list, "arbjhy", "R.JJ.J"), false);
}

test "check" {
  var list = std.ArrayList(u8).init(std.testing.allocator);
  defer list.deinit();

  {
    const res = try check(std.testing.allocator, "ABCDEF", "BACEDF");
    defer std.testing.allocator.free(res);
    try std.testing.expectEqualStrings("JJRJJR", res);
  }
  {
    const res = try check(std.testing.allocator, "ABCDEF", "QWERTY");
    defer std.testing.allocator.free(res);
    try std.testing.expectEqualStrings("..J...", res);
  }
  {
    const res = try check(std.testing.allocator, "PEPLUM", "PLUMEE");
    defer std.testing.allocator.free(res);
    try std.testing.expectEqualStrings("RJJJJ.", res);
  }
}
