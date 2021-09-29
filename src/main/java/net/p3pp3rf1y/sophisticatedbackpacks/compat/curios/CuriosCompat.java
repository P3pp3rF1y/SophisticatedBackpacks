package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.InterModComms;
import net.minecraftforge.fml.event.lifecycle.InterModEnqueueEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import top.theillusivec4.curios.api.CuriosApi;
import top.theillusivec4.curios.api.CuriosCapability;
import top.theillusivec4.curios.api.SlotTypeMessage;
import top.theillusivec4.curios.api.SlotTypePreset;
import top.theillusivec4.curios.api.client.CuriosRendererRegistry;
import top.theillusivec4.curios.api.type.inventory.ICurioStacksHandler;

import javax.annotation.Nullable;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

public class CuriosCompat implements ICompat {
	public CuriosCompat() {
		IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
		modEventBus.addListener(this::sendImc);
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			CuriosRendererRegistry.register(ModItems.BACKPACK.get(), BackpackCurioRenderer::new);
			CuriosRendererRegistry.register(ModItems.IRON_BACKPACK.get(), BackpackCurioRenderer::new);
			CuriosRendererRegistry.register(ModItems.GOLD_BACKPACK.get(), BackpackCurioRenderer::new);
			CuriosRendererRegistry.register(ModItems.DIAMOND_BACKPACK.get(), BackpackCurioRenderer::new);
			CuriosRendererRegistry.register(ModItems.NETHERITE_BACKPACK.get(), BackpackCurioRenderer::new);
		});

		MinecraftForge.EVENT_BUS.addGenericListener(ItemStack.class, this::onAttachCapabilities);

		PlayerInventoryProvider.get().setPlayerInventoryHandlerInitCallback(player -> CuriosApi.getCuriosHelper().getCuriosHandler(player).ifPresent(handler -> {
			Set<String> backpackCurioTags = CuriosApi.getCuriosHelper().getCurioTags(ModItems.BACKPACK.get());
			for (String identifier : handler.getCurios().keySet()) {
				if (identifier.equals(SlotTypePreset.CURIO.getIdentifier()) || backpackCurioTags.contains(identifier)) {
					addSlotProvider(identifier);
				}
			}
		}));
	}

	private void addSlotProvider(String identifier) {
		PlayerInventoryProvider.get().addPlayerInventoryHandler(CompatModIds.CURIOS + "_" + identifier,
				player -> getFromCuriosSlotStackHandler(player, identifier, ICurioStacksHandler::getSlots, 0),
				(player, slot) -> getFromCuriosSlotStackHandler(player, identifier, sh -> sh.getStacks().getStackInSlot(slot), ItemStack.EMPTY),
				(player, slot, stack) -> runOnBackStackHandler(player, identifier, sh -> sh.getStacks().setStackInSlot(slot, stack)), false, true, true
		);
	}

	public static <T> T getFromCuriosSlotStackHandler(LivingEntity livingEntity, String identifier, Function<ICurioStacksHandler, T> getFromHandler, T defaultValue) {
		return CuriosApi.getCuriosHelper().getCuriosHandler(livingEntity)
				.map(h -> h.getStacksHandler(identifier).map(getFromHandler).orElse(defaultValue)).orElse(defaultValue);
	}

	private void runOnBackStackHandler(Player player, String identifier, Consumer<ICurioStacksHandler> runOnHandler) {
		CuriosApi.getCuriosHelper().getCuriosHandler(player)
				.ifPresent(h -> h.getStacksHandler(identifier).ifPresent(runOnHandler));
	}

	private void sendImc(InterModEnqueueEvent evt) {
		InterModComms.sendTo(CompatModIds.CURIOS, SlotTypeMessage.REGISTER_TYPE, () -> SlotTypePreset.BACK.getMessageBuilder().build());
	}

	public void onAttachCapabilities(AttachCapabilitiesEvent<ItemStack> evt) {
		ItemStack stack = evt.getObject();
		Item item = stack.getItem();
		if (item.getRegistryName() != null && item.getRegistryName().getNamespace().equals(SophisticatedBackpacks.MOD_ID) && item instanceof BackpackItem) {
			evt.addCapability(new ResourceLocation(SophisticatedBackpacks.MOD_ID, item.getRegistryName().getPath() + "_curios"), new ICapabilityProvider() {
				@Override
				public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
					return CuriosCapability.ITEM.orEmpty(cap, LazyOptional.of(() -> () -> stack));
				}
			});
		}
	}

	@Override
	public void setup() {
		//noop
	}
}
