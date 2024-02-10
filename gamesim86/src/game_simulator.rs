use std::{thread, time};

use sim8086::sim::register_memory::Register;
use sim8086::{Simulator, SimulatorDelegate, InterruptErr};

use show_image::{ImageView, ImageInfo, create_window, WindowOptions, WindowProxy};
use sim8086::sim::simulation::SimulateErr;

pub struct GameSimulator {
    window: WindowProxy,
    frame: usize,
    start_time: time::Instant,
    time: f32,
    pub frame_time: f32,
    pub frame_width: u16,
    pub frame_height: u16,
}

impl GameSimulator {
    // Special memory address at which input data is written
    const INPUT_ADDR: usize = 0xFFFE;

    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let window_options = WindowOptions::new()
            .set_size([768, 768])
            .set_preserve_aspect_ratio(false)
            .set_default_controls(false);

        let window = create_window("Game", window_options)?;

        // Need to flip image to match framebuffer layout
        /*window.run_function(|mut handle| {
            //let transform = Affine2::from_scale(Vec2 { x: 1., y: -1. });
            let transform = Affine2::from_scale_angle_translation(
                Vec2 { x: 0.1, y: -0.1 }, 
                0., 
                Vec2 { x: 0.5, y: 0.5 }
            );
            //let transform = Affine2::from_cols(Vec2::X, -Vec2::Y, Vec2::ZERO);
            
            handle.set_transform(transform)
        });*/

        Ok(Self {
            window,
            frame: 0,
            start_time: time::Instant::now(),
            time: 0f32,
            frame_time: 1. / 10.,
            frame_width: 64,
            frame_height: 64,
        })
    }

    pub fn get_framebuffer_size(&self) -> usize {
        (self.frame_width * self.frame_height * 4) as usize
    }
}

impl SimulatorDelegate for GameSimulator {
    fn simulate(&mut self, simulator: &mut Simulator) -> Result<(), SimulateErr>{
        use device_query::{DeviceQuery, DeviceState, Keycode};

        let device_state = DeviceState::new();
        let keys = device_state.get_keys();

        let mut input: u8 = 0;
        for key in keys.iter() {
            input |= match key {
                Keycode::Up => 1 << 0,
                Keycode::Down => 1 << 1,
                Keycode::Left => 1 << 2,
                Keycode::Right => 1 << 3,
                Keycode::Space => 1 << 4,
                Keycode::Escape => return Err(SimulateErr::Halt),
                _ => 0
            }
        }

        // Write to special input addr
        simulator.write_byte_mem(Self::INPUT_ADDR, input);
        
        Ok(())
    }

    fn interrupt_handler(&mut self, simulator: &mut Simulator, code: u8) -> Result<(), InterruptErr> {
        match code {
            0x0 => {
                // Set up snake framebuffer size: Width is put in ax, height into bx.
                simulator.register_storage.write_register(Register::AX, self.frame_width);
                simulator.register_storage.write_register(Register::BX, self.frame_height);
                Ok(())
            }
            0x3 => {
                // int3 used as halt
                Err(InterruptErr::Halt)
            }
            0x15 => {
                // Frame update interrupt

                // Display frame
                let framebuffer_start = simulator.get_program_size();
                let framebuffer_end = framebuffer_start + self.get_framebuffer_size();

                let framebuffer = &simulator.get_memory()[framebuffer_start..framebuffer_end];

                // Issue: framebuffer is displayed y-flipped by show-image. Need to copy and flip
                let flipped: Vec<u8> = framebuffer.rchunks_exact(self.frame_width as usize * 4).flatten().cloned().collect();
                
                let image = ImageView::new(ImageInfo::rgba8(self.frame_width as u32, self.frame_height as u32), &flipped);
                let has_quit = self.window.set_image("frame", image).is_err();

                if has_quit {
                    return Err(InterruptErr::Halt);
                }

                // Update frame time
                let new_time = self.start_time.elapsed().as_secs_f32();
                let delta_time = new_time - self.time;

                let sleep_time = self.frame_time - delta_time;
                if sleep_time > 0. {
                    thread::sleep(time::Duration::from_secs_f32(sleep_time))
                } else {
                    eprintln!("Warning: target framerate not achieved")
                }

                self.time = self.start_time.elapsed().as_secs_f32();

                self.frame += 1;

                Ok(())
            }
            _ => Err(InterruptErr::Unhandled),
        }
    }
}
