#include <TFile.h>
#include <TTree.h>
#include <cstdlib>

extern "C"
void* open_ntuple(const char* file_name) {
  TFile* file = new TFile(file_name,"RECREATE");
  if (file->IsZombie()) exit(1);
  return new TTree("ntuple","");
}

extern "C"
void close_ntuple(void* ntuple) {
  TFile* file = reinterpret_cast<TFile*>(
    reinterpret_cast<TTree*>(ntuple)->GetDirectory());
  file->Write(0,TObject::kOverwrite);
  delete file;
}

extern "C"
void fill_ntuple(void* ntuple) {
  reinterpret_cast<TTree*>(ntuple)->Fill();
}

#define add_ntuple_branch(TYPE) \
extern "C" \
void add_ntuple_branch_##TYPE(void* ntuple, const char* name, TYPE* ptr) { \
  reinterpret_cast<TTree*>(ntuple)->Branch(name,ptr); \
}

add_ntuple_branch(double)
add_ntuple_branch(float)
add_ntuple_branch(int)

