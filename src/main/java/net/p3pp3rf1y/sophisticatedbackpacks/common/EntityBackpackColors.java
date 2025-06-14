package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.world.entity.EntityType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;

import java.util.HashMap;
import java.util.Map;

public class EntityBackpackColors {
	private static final Map<EntityType<?>, BackpackColors> ENTITY_COLORS = new HashMap<>();
	static {
		ENTITY_COLORS.put(EntityType.ARMADILLO, new BackpackColors(11366765, 8538184));
		ENTITY_COLORS.put(EntityType.ALLAY, new BackpackColors(56063, 44543));
		ENTITY_COLORS.put(EntityType.AXOLOTL, new BackpackColors(16499171, 10890612));
		ENTITY_COLORS.put(EntityType.BAT, new BackpackColors(4996656, 986895));
		ENTITY_COLORS.put(EntityType.BEE, new BackpackColors(15582019, 4400155));
		ENTITY_COLORS.put(EntityType.BLAZE, new BackpackColors(16167425, 16775294));
		ENTITY_COLORS.put(EntityType.BOGGED, new BackpackColors(9084018, 3231003));
		ENTITY_COLORS.put(EntityType.BREEZE, new BackpackColors(11506911, 9529055));
		ENTITY_COLORS.put(EntityType.CAT, new BackpackColors(15714446, 9794134));
		ENTITY_COLORS.put(EntityType.CAMEL, new BackpackColors(16565097, 13341495));
		ENTITY_COLORS.put(EntityType.CAVE_SPIDER, new BackpackColors(803406, 11013646));
		ENTITY_COLORS.put(EntityType.CHICKEN, new BackpackColors(10592673, 16711680));
		ENTITY_COLORS.put(EntityType.COD, new BackpackColors(12691306, 15058059));
		ENTITY_COLORS.put(EntityType.COW, new BackpackColors(4470310, 10592673));
		ENTITY_COLORS.put(EntityType.CREEPER, new BackpackColors(894731, 0));
		ENTITY_COLORS.put(EntityType.DOLPHIN, new BackpackColors(2243405, 16382457));
		ENTITY_COLORS.put(EntityType.DONKEY, new BackpackColors(5457209, 8811878));
		ENTITY_COLORS.put(EntityType.DROWNED, new BackpackColors(9433559, 7969893));
		ENTITY_COLORS.put(EntityType.ELDER_GUARDIAN, new BackpackColors(13552826, 7632531));
		ENTITY_COLORS.put(EntityType.ENDER_DRAGON, new BackpackColors(1842204, 14711290));
		ENTITY_COLORS.put(EntityType.ENDERMAN, new BackpackColors(1447446, 0));
		ENTITY_COLORS.put(EntityType.ENDERMITE, new BackpackColors(1447446, 7237230));
		ENTITY_COLORS.put(EntityType.EVOKER, new BackpackColors(9804699, 1973274));
		ENTITY_COLORS.put(EntityType.FOX, new BackpackColors(14005919, 13396256));
		ENTITY_COLORS.put(EntityType.FROG, new BackpackColors(13661252, 16762748));
		ENTITY_COLORS.put(EntityType.GHAST, new BackpackColors(16382457, 12369084));
		ENTITY_COLORS.put(EntityType.GLOW_SQUID, new BackpackColors(611926, 8778172));
		ENTITY_COLORS.put(EntityType.GOAT, new BackpackColors(10851452, 5589310));
		ENTITY_COLORS.put(EntityType.GUARDIAN, new BackpackColors(5931634, 15826224));
		ENTITY_COLORS.put(EntityType.HOGLIN, new BackpackColors(13004373, 6251620));
		ENTITY_COLORS.put(EntityType.HORSE, new BackpackColors(12623485, 15656192));
		ENTITY_COLORS.put(EntityType.HUSK, new BackpackColors(7958625, 15125652));
		ENTITY_COLORS.put(EntityType.IRON_GOLEM, new BackpackColors(14405058, 7643954));
		ENTITY_COLORS.put(EntityType.LLAMA, new BackpackColors(12623485, 10051392));
		ENTITY_COLORS.put(EntityType.MAGMA_CUBE, new BackpackColors(3407872, 16579584));
		ENTITY_COLORS.put(EntityType.MOOSHROOM, new BackpackColors(10489616, 12040119));
		ENTITY_COLORS.put(EntityType.MULE, new BackpackColors(1769984, 5321501));
		ENTITY_COLORS.put(EntityType.OCELOT, new BackpackColors(15720061, 5653556));
		ENTITY_COLORS.put(EntityType.PANDA, new BackpackColors(15198183, 1776418));
		ENTITY_COLORS.put(EntityType.PARROT, new BackpackColors(894731, 16711680));
		ENTITY_COLORS.put(EntityType.PHANTOM, new BackpackColors(4411786, 8978176));
		ENTITY_COLORS.put(EntityType.PIG, new BackpackColors(15771042, 14377823));
		ENTITY_COLORS.put(EntityType.PIGLIN, new BackpackColors(10051392, 16380836));
		ENTITY_COLORS.put(EntityType.PIGLIN_BRUTE, new BackpackColors(5843472, 16380836));
		ENTITY_COLORS.put(EntityType.PILLAGER, new BackpackColors(5451574, 9804699));
		ENTITY_COLORS.put(EntityType.POLAR_BEAR, new BackpackColors(15658718, 14014157));
		ENTITY_COLORS.put(EntityType.PUFFERFISH, new BackpackColors(16167425, 3654642));
		ENTITY_COLORS.put(EntityType.RABBIT, new BackpackColors(10051392, 7555121));
		ENTITY_COLORS.put(EntityType.RAVAGER, new BackpackColors(7697520, 5984329));
		ENTITY_COLORS.put(EntityType.SALMON, new BackpackColors(10489616, 951412));
		ENTITY_COLORS.put(EntityType.SHEEP, new BackpackColors(15198183, 16758197));
		ENTITY_COLORS.put(EntityType.SHULKER, new BackpackColors(9725844, 5060690));
		ENTITY_COLORS.put(EntityType.SILVERFISH, new BackpackColors(7237230, 3158064));
		ENTITY_COLORS.put(EntityType.SKELETON, new BackpackColors(12698049, 4802889));
		ENTITY_COLORS.put(EntityType.SKELETON_HORSE, new BackpackColors(6842447, 15066584));
		ENTITY_COLORS.put(EntityType.SLIME, new BackpackColors(5349438, 8306542));
		ENTITY_COLORS.put(EntityType.SNIFFER, new BackpackColors(8855049, 2468720));
		ENTITY_COLORS.put(EntityType.SNOW_GOLEM, new BackpackColors(14283506, 8496292));
		ENTITY_COLORS.put(EntityType.SPIDER, new BackpackColors(3419431, 11013646));
		ENTITY_COLORS.put(EntityType.SQUID, new BackpackColors(2243405, 7375001));
		ENTITY_COLORS.put(EntityType.STRAY, new BackpackColors(6387319, 14543594));
		ENTITY_COLORS.put(EntityType.STRIDER, new BackpackColors(10236982, 5065037));
		ENTITY_COLORS.put(EntityType.TADPOLE, new BackpackColors(7164733, 1444352));
		ENTITY_COLORS.put(EntityType.TRADER_LLAMA, new BackpackColors(15377456, 4547222));
		ENTITY_COLORS.put(EntityType.TROPICAL_FISH, new BackpackColors(15690005, 16775663));
		ENTITY_COLORS.put(EntityType.TURTLE, new BackpackColors(15198183, 44975));
		ENTITY_COLORS.put(EntityType.VEX, new BackpackColors(8032420, 15265265));
		ENTITY_COLORS.put(EntityType.VILLAGER, new BackpackColors(5651507, 12422002));
		ENTITY_COLORS.put(EntityType.VINDICATOR, new BackpackColors(9804699, 2580065));
		ENTITY_COLORS.put(EntityType.WANDERING_TRADER, new BackpackColors(4547222, 15377456));
		ENTITY_COLORS.put(EntityType.WARDEN, new BackpackColors(1001033, 3790560));
		ENTITY_COLORS.put(EntityType.WITCH, new BackpackColors(3407872, 5349438));
		ENTITY_COLORS.put(EntityType.WITHER, new BackpackColors(1315860, 5075616));
		ENTITY_COLORS.put(EntityType.WITHER_SKELETON, new BackpackColors(1315860, 4672845));
		ENTITY_COLORS.put(EntityType.WOLF, new BackpackColors(14144467, 13545366));
		ENTITY_COLORS.put(EntityType.ZOGLIN, new BackpackColors(13004373, 15132390));
		ENTITY_COLORS.put(EntityType.CREAKING, new BackpackColors(6250335, 16545810));
		ENTITY_COLORS.put(EntityType.ZOMBIE, new BackpackColors(44975, 7969893));
		ENTITY_COLORS.put(EntityType.ZOMBIE_HORSE, new BackpackColors(3232308, 9945732));
		ENTITY_COLORS.put(EntityType.ZOMBIE_VILLAGER, new BackpackColors(5651507, 7969893));
		ENTITY_COLORS.put(EntityType.ZOMBIFIED_PIGLIN, new BackpackColors(15373203, 5009705));
	}

	public static BackpackColors getBackpackColors(EntityType<?> entityType) {
		return ENTITY_COLORS.getOrDefault(entityType, new BackpackColors(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR));
	}

	public record BackpackColors(int main, int accent) {}
}
