use atma::db::AdsEnvironment;
use atma::error::StrSrcCache;
use atma::system::SimSystem;
use std::io::Write;
use std::rc::Rc;

//===========================================================================//

fn make_nop_proc_env<'a>(
    source: &str,
    output: &'a mut Vec<u8>,
) -> AdsEnvironment<&'a mut Vec<u8>> {
    let mut cache = StrSrcCache::new();
    let path = Rc::<str>::from("input");
    let bus = atma::bus::new_ram_bus(Box::new([0u8; 0x100]));
    let cpu = atma::proc::NopProc::new();
    let sim = SimSystem::new(vec![(Rc::from("cpu"), (Box::new(cpu), bus))]);
    AdsEnvironment::create(&mut cache, path, source, sim, output).unwrap()
}

fn run_to_completion<W: Write>(env: &mut AdsEnvironment<W>) {
    loop {
        match env.step() {
            Ok(true) => break,
            Ok(false) => {}
            Err(err) => panic!("{err:?}"),
        }
    }
}

fn compile_and_run(source: &str) -> String {
    let mut output = Vec::<u8>::new();
    let mut ads = make_nop_proc_env(source, &mut output);
    run_to_completion(&mut ads);
    String::from_utf8(output).unwrap()
}

//===========================================================================//
// Expressions:

#[test]
fn print_statement() {
    assert_eq!(compile_and_run("print 42\n"), "42\n");
}

#[test]
fn byte_selection_operator() {
    assert_eq!(compile_and_run("print $090507 ! 0\n"), "7\n");
    assert_eq!(compile_and_run("print $090507 ! 1\n"), "5\n");
    assert_eq!(compile_and_run("print $090507 ! 2\n"), "9\n");
    assert_eq!(compile_and_run("print $090507 ! 3\n"), "0\n");
    assert_eq!(compile_and_run("print -$fcf4f8 ! 0\n"), "8\n");
    assert_eq!(compile_and_run("print -$fcf4f8 ! 1\n"), "11\n");
    assert_eq!(compile_and_run("print -$fcf4f8 ! 2\n"), "3\n");
    assert_eq!(compile_and_run("print -$fcf4f8 ! 3\n"), "255\n");
}

#[test]
fn length_operator() {
    assert_eq!(compile_and_run("print {1, 2, 4, 8, 16}#\n"), "5\n");
    assert_eq!(compile_and_run("print {{}, {1, 2}}[1]#\n"), "2\n");
}

//===========================================================================//
// Variables:

#[test]
fn get_and_set_pc() {
    // Unlike user-defined variables, "PC" is case-insensitive.
    let source = r#"\
      run until at $10
      print pc
      set Pc = $20
      print pC
    "#;
    assert_eq!(compile_and_run(source), "16\n32\n");
}

#[test]
fn tuple_lvalue() {
    let source = r#"\
      var integer = 0
      var list = {0}
      let tuple = (1, "2", {3, 4})
      set (integer, _, list) = tuple
      print (integer, list)
    "#;
    assert_eq!(compile_and_run(source), "(1, {3, 4})\n");
}

#[test]
fn memory_lvalue() {
    let source = r#"\
      set [$00] = 37
      set [$01] = 42
      when read 0 {
        print DATA
      }
      when read 1 {
        print DATA
      }
      step
      step
    "#;
    assert_eq!(compile_and_run(source), "37\n42\n");
}

//===========================================================================//
// Control flow:

#[test]
fn for_loop() {
    let source = r#"\
      var sum = 0
      for value <- {1, 2, 3, 4} {
        print value
        set sum = sum + value
      }
      print sum
    "#;
    assert_eq!(compile_and_run(source), "1\n2\n3\n4\n10\n");
}

//===========================================================================//
// Handlers:

#[test]
fn when_handler() {
    let source = r#"\
      when at $01 {
        print 2
      }
      print 1
      step
      print 3
    "#;
    assert_eq!(compile_and_run(source), "1\n2\n3\n");
}

#[test]
fn when_handler_with_local_variable() {
    let source = r#"\
      var x = 1
      when at $01 {
        var y = 2
        print x
        print y
      }
      var z = 3
      step
      print z
    "#;
    assert_eq!(compile_and_run(source), "1\n2\n3\n");
}

#[test]
fn run_until_statement() {
    let source = r#"\
      var x = 1
      when at $10 {
        set x = 2
      }
      run until at $20
      print x
    "#;
    assert_eq!(compile_and_run(source), "2\n");
}

#[test]
fn nested_handlers() {
    let source = r#"\
      when at $10 {
        print 1
        var x = 2
        when at $20 {
          print x
          set x = 3
        }
        run until at $30
        print x
      }
      run until at $40
      print 4
    "#;
    assert_eq!(compile_and_run(source), "1\n2\n3\n4\n");
}

#[test]
fn mid_instruction_handler() {
    let source = r#"\
      when read $00 {
        print pc  ; still mid-instruction, so PC has not advanced yet
      }
      step        ; read and execute the NOP at $00
      print pc    ; now PC has advanced
    "#;
    assert_eq!(compile_and_run(source), "0\n1\n");
}

//===========================================================================//
