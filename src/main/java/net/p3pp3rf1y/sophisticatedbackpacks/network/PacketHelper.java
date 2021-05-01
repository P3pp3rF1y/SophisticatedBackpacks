package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;

public class PacketHelper {
	private PacketHelper() {}

	public static ItemStack readItemStack(PacketBuffer packetBuffer) {
		if (!packetBuffer.readBoolean()) {
			return ItemStack.EMPTY;
		} else {
			int i = packetBuffer.readVarInt();
			int j = packetBuffer.readInt();
			ItemStack itemstack = new ItemStack(Item.getItemById(i), j);
			itemstack.readShareTag(packetBuffer.readCompoundTag());
			return itemstack;
		}
	}

	public static void writeItemStack(ItemStack stack, PacketBuffer packetBuffer) {
		if (stack.isEmpty()) {
			packetBuffer.writeBoolean(false);
		} else {
			packetBuffer.writeBoolean(true);
			Item item = stack.getItem();
			packetBuffer.writeVarInt(Item.getIdFromItem(item));
			packetBuffer.writeInt(stack.getCount());
			CompoundNBT compoundnbt = null;
			if (item.isDamageable(stack) || item.shouldSyncTag()) {
				compoundnbt = stack.getShareTag();
			}

			packetBuffer.writeCompoundTag(compoundnbt);
		}
	}
}
