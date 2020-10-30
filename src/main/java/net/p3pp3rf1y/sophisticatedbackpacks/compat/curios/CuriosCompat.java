package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Direction;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.AttachCapabilitiesEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.InterModComms;
import net.minecraftforge.fml.event.lifecycle.InterModEnqueueEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import top.theillusivec4.curios.api.CuriosApi;
import top.theillusivec4.curios.api.CuriosCapability;
import top.theillusivec4.curios.api.SlotTypeMessage;
import top.theillusivec4.curios.api.SlotTypePreset;
import top.theillusivec4.curios.api.type.inventory.ICurioStacksHandler;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.Consumer;
import java.util.function.Function;

public class CuriosCompat implements ICompat {
	public CuriosCompat() {
		IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
		modEventBus.addListener(this::sendImc);
		MinecraftForge.EVENT_BUS.addGenericListener(ItemStack.class, this::onAttachCapabilities);
		PlayerInventoryProvider.addPlayerInventoryHandler(CompatModIds.CURIOS,
				player -> getFromBackStackHandler(player, ICurioStacksHandler::getSlots, 0),
				(player, slot) -> getFromBackStackHandler(player, sh -> sh.getStacks().getStackInSlot(slot), ItemStack.EMPTY),
				false,
				(player, slot, stack) -> runOnBackStackHandler(player, sh -> sh.getStacks().setStackInSlot(slot, stack)));
	}

	private <T> T getFromBackStackHandler(PlayerEntity player, Function<ICurioStacksHandler, T> getFromHandler, T defaultValue) {
		return CuriosApi.getCuriosHelper().getCuriosHandler(player)
				.map(h -> h.getStacksHandler(SlotTypePreset.BACK.getIdentifier()).map(getFromHandler).orElse(defaultValue)).orElse(defaultValue);
	}

	private void runOnBackStackHandler(PlayerEntity player, Consumer<ICurioStacksHandler> runOnHandler) {
		CuriosApi.getCuriosHelper().getCuriosHandler(player)
				.ifPresent(h -> h.getStacksHandler(SlotTypePreset.BACK.getIdentifier()).ifPresent(runOnHandler));
	}

	private void sendImc(InterModEnqueueEvent evt) {
		InterModComms.sendTo(CompatModIds.CURIOS, SlotTypeMessage.REGISTER_TYPE, () -> SlotTypePreset.BACK.getMessageBuilder().build());
	}

	public void onAttachCapabilities(AttachCapabilitiesEvent<ItemStack> evt) {
		ItemStack stack = evt.getObject();
		Item item = stack.getItem();
		if (item.getRegistryName() != null && item.getRegistryName().getNamespace().equals(SophisticatedBackpacks.MOD_ID) && item instanceof BackpackItem) {
			evt.addCapability(new ResourceLocation(SophisticatedBackpacks.MOD_ID, item.getRegistryName().getPath() + "_curios"), new ICapabilityProvider() {
				@Nonnull
				@Override
				public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
					return CuriosCapability.ITEM.orEmpty(cap, LazyOptional.of(() -> new CuriosBackpackWrapper(item)));
				}
			});
		}
	}

	@Override
	public void setup() {
		//noop
	}
}
