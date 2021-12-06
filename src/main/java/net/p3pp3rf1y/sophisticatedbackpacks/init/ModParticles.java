package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.client.Minecraft;
import net.minecraft.core.particles.ParticleType;
import net.minecraftforge.client.event.ParticleFactoryRegisterEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeNoteParticle;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeNoteParticleData;

public class ModParticles {
	private ModParticles() {}

	private static final DeferredRegister<ParticleType<?>> PARTICLES = DeferredRegister.create(ForgeRegistries.PARTICLE_TYPES, SophisticatedBackpacks.MOD_ID);

	public static final RegistryObject<JukeboxUpgradeNoteParticleData> JUKEBOX_NOTE = PARTICLES.register("jukebox_note", JukeboxUpgradeNoteParticleData::new);

	public static void registerParticles(IEventBus modBus) {
		PARTICLES.register(modBus);
	}

	@SuppressWarnings("unused") // need this to register the event correctly
	public static void registerFactories(ParticleFactoryRegisterEvent event) {
		Minecraft.getInstance().particleEngine.register(JUKEBOX_NOTE.get(), JukeboxUpgradeNoteParticle.Factory::new);
	}
}
