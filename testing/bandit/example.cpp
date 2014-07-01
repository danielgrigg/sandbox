#include <bandit/bandit.h>
#include <memory>
#include <iostream>

using namespace bandit;
using namespace std;

struct fuzzbox {

  void flip() { _flipped = true;}
  bool _flipped = false;
};

enum class sounds {
  clean,
  distorted
};

struct guitar {
  void add_effect(fuzzbox& f) { _effect = &f; }
  sounds sound() { return _effect != nullptr && _effect->_flipped ? sounds::distorted : sounds::clean; }

  fuzzbox* _effect;
};

typedef std::unique_ptr<guitar> guitar_ptr;
typedef std::unique_ptr<fuzzbox> fuzzbox_ptr;

// Tell bandit there are tests here.
go_bandit([](){

    // We're describing how a fuzzbox works.
    describe("fuzzbox:", [](){
      guitar_ptr guitar;
      fuzzbox_ptr fuzzbox;

      // Make sure each test has a fresh setup with
      // a guitar with a fuzzbox connected to it.
      before_each([&](){
        guitar = guitar_ptr(new struct guitar());
        fuzzbox = fuzzbox_ptr(new struct fuzzbox());
        guitar->add_effect(*fuzzbox);
      });

      it("starts in clean mode", [&](){
        AssertThat(guitar->sound(), Equals(sounds::clean));
      });

      // Describe what happens when we turn on the fuzzbox.
      describe("in distorted mode", [&](){

        // Turn on the fuzzbox.
        before_each([&](){
          fuzzbox->flip();
        });

        it("sounds distorted", [&](){
          AssertThat(guitar->sound(), Equals(sounds::distorted));
        });
      });
    });

});

int main(int argc, char* argv[])
{
  // Run the tests.
  return bandit::run(argc, argv);
}
